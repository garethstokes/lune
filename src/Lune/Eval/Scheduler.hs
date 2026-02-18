module Lune.Eval.Scheduler
  ( SchedulerState (..)
  , FiberEntry (..)
  , newScheduler
  , spawnFiber
  , runScheduler
  ) where

import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Lune.Eval.Types
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- | Entry for a fiber in the scheduler
data FiberEntry = FiberEntry
  { fiberCont :: World -> IO (Either EvalError IOStep)
    -- ^ Continuation to resume this fiber
  , fiberWaiters :: [FiberId]
    -- ^ Fibers waiting for this one to complete
  }

-- | Scheduler state - mutable via IORef
data SchedulerState = SchedulerState
  { schedFibers :: IORef (IntMap FiberEntry)
    -- ^ All fiber entries by ID
  , schedReadyQueue :: IORef (Seq FiberId)
    -- ^ Fibers ready to run (FIFO)
  , schedSleeping :: IORef [(Int, FiberId, World -> IO (Either EvalError IOStep))]
    -- ^ Sleeping fibers: (wake time in ms, fiber id, continuation)
  , schedNextId :: IORef FiberId
    -- ^ Next fiber ID to allocate
  , schedResults :: IORef (IntMap Value)
    -- ^ Completed fiber results (for await)
  }

-- | Create a new scheduler
newScheduler :: IO SchedulerState
newScheduler = do
  fibers <- newIORef IntMap.empty
  ready <- newIORef Seq.empty
  sleeping <- newIORef []
  nextId <- newIORef 0
  results <- newIORef IntMap.empty
  pure SchedulerState
    { schedFibers = fibers
    , schedReadyQueue = ready
    , schedSleeping = sleeping
    , schedNextId = nextId
    , schedResults = results
    }

-- | Spawn a new fiber, returns its ID
spawnFiber :: SchedulerState -> (World -> IO (Either EvalError IOStep)) -> IO FiberId
spawnFiber sched cont = do
  fid <- atomicModifyIORef' (schedNextId sched) (\n -> (n + 1, n))
  let entry = FiberEntry cont []
  modifyIORef' (schedFibers sched) (IntMap.insert fid entry)
  modifyIORef' (schedReadyQueue sched) (Seq.|> fid)
  pure fid

-- | Get current time in milliseconds
nowMs :: IO Int
nowMs = do
  t <- getPOSIXTime
  pure $ floor (t * 1000)

-- | Run the scheduler until all fibers complete
-- Returns the result of the main fiber (fiber 0)
runScheduler :: SchedulerState -> World -> IO (Either EvalError (World, Value))
runScheduler sched world0 = loop world0
  where
    loop world = do
      -- Wake any sleeping fibers that are due
      wakeSlept sched

      -- Try to get next ready fiber
      ready <- atomicModifyIORef' (schedReadyQueue sched) $ \q ->
        case Seq.viewl q of
          Seq.EmptyL -> (q, Nothing)
          fid Seq.:< rest -> (rest, Just fid)

      case ready of
        Nothing -> do
          -- No ready fibers - check if any sleeping
          sleeping <- readIORef (schedSleeping sched)
          if null sleeping
            then do
              -- All done - get main fiber result
              results <- readIORef (schedResults sched)
              case IntMap.lookup 0 results of
                Just v -> pure $ Right (world, v)
                Nothing -> pure $ Left (UnboundVariable "main fiber did not complete")
            else do
              -- Sleep until next wakeup
              let (nextWake, _, _) = head sleeping
              now <- nowMs
              let delay = max 0 (nextWake - now)
              when (delay > 0) $
                threadDelay (delay * 1000)  -- microseconds
              loop world

        Just fid -> do
          -- Run this fiber for one step
          fibers <- readIORef (schedFibers sched)
          case IntMap.lookup fid fibers of
            Nothing -> loop world  -- Fiber was removed
            Just entry -> do
              result <- fiberCont entry world
              case result of
                Left err -> pure $ Left err
                Right step -> handleStep sched fid step >>= loop

-- | Wake sleeping fibers that are due
wakeSlept :: SchedulerState -> IO ()
wakeSlept sched = do
  now <- nowMs
  (due, remaining) <- atomicModifyIORef' (schedSleeping sched) $ \sleeping ->
    let (due', remaining') = span (\(wake, _, _) -> wake <= now) sleeping
    in (remaining', (due', remaining'))
  -- Add woken fibers back to ready queue
  mapM_ (\(_, fid, cont) -> do
    modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = cont }) fid)
    modifyIORef' (schedReadyQueue sched) (Seq.|> fid)
    ) due

-- | Handle a step result from a fiber
handleStep :: SchedulerState -> FiberId -> IOStep -> IO World
handleStep sched fid step =
  case step of
    StepDone world value -> do
      -- Fiber completed - store result and wake waiters
      modifyIORef' (schedResults sched) (IntMap.insert fid value)
      fibers <- readIORef (schedFibers sched)
      case IntMap.lookup fid fibers of
        Nothing -> pure ()
        Just entry -> do
          -- Wake all waiters
          forM_ (fiberWaiters entry) $ \waiterId -> do
            modifyIORef' (schedReadyQueue sched) (Seq.|> waiterId)
      -- Remove completed fiber
      modifyIORef' (schedFibers sched) (IntMap.delete fid)
      pure world

    StepYield world cont -> do
      -- Put fiber at end of ready queue
      modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = cont }) fid)
      modifyIORef' (schedReadyQueue sched) (Seq.|> fid)
      pure world

    StepSleep world ms cont -> do
      -- Add to sleeping list (sorted by wake time)
      now <- nowMs
      let wakeAt = now + ms
      modifyIORef' (schedSleeping sched) $ \sleeping ->
        insertSorted wakeAt fid cont sleeping
      -- Update fiber entry (it's not in ready queue)
      modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = cont }) fid)
      pure world

    StepAwait world targetFid cont -> do
      -- Check if target already completed
      results <- readIORef (schedResults sched)
      case IntMap.lookup targetFid results of
        Just value -> do
          -- Target already done - resume immediately with value
          modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) fid)
          modifyIORef' (schedReadyQueue sched) (Seq.|> fid)
          pure world
        Nothing -> do
          -- Add self to target's waiters list
          modifyIORef' (schedFibers sched) $ \fibers ->
            IntMap.adjust (\e -> e { fiberWaiters = fid : fiberWaiters e }) targetFid $
            IntMap.adjust (\e -> e { fiberCont = \w -> cont w (error "await placeholder") }) fid fibers
          -- Fiber stays out of ready queue until target completes
          pure world

    StepSpawn world newCont resumeCont -> do
      -- Spawn new fiber
      newFid <- spawnFiber sched newCont
      -- Resume spawner with the new fiber ID
      modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = \w -> resumeCont w newFid }) fid)
      modifyIORef' (schedReadyQueue sched) (Seq.|> fid)
      pure world

-- Helper to insert into sorted sleeping list
insertSorted :: Int -> FiberId -> (World -> IO (Either EvalError IOStep)) -> [(Int, FiberId, World -> IO (Either EvalError IOStep))] -> [(Int, FiberId, World -> IO (Either EvalError IOStep))]
insertSorted wake fid' cont [] = [(wake, fid', cont)]
insertSorted wake fid' cont (x@(w, _, _) : xs)
  | wake <= w = (wake, fid', cont) : x : xs
  | otherwise = x : insertSorted wake fid' cont xs
