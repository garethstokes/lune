module Lune.Eval.Scheduler
  ( SchedulerState (..)
  , FiberEntry (..)
  , newScheduler
  , spawnFiber
  , runScheduler
  , ParSchedulerState (..)
  , newParScheduler
  , spawnFiberPar
  , handleStepPar
  , tryDequeue
  , workerThreadWithWorld
  , sleepManager
  , wakeSleptPar
  , runParScheduler
  ) where

import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, replicateM_, forever, void, when)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lune.Eval.Types

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
  , schedAwaitConts :: IORef (IntMap (World -> Value -> IO (Either EvalError IOStep)))
    -- ^ Stored await continuations by waiter fiber ID
  , schedMainDone :: IORef (Maybe (World, Value))
    -- ^ Result of main fiber (fiber 0), when completed
  }

{-| M:N Scheduler Architecture

The parallel scheduler runs M Lune fibers on N OS threads:

World State Handling:
- Each fiber step receives a copy of the initial World
- SharedState (TVars) is thread-safe via STM - fibers share this
- Stdout uses direct IO (TIO.putStrLn) which is thread-safe
- Socket/TLS maps are not shared; each fiber step operates atomically

Thread Model:
- Global ready queue (TVar (Seq FiberId))
- N-1 worker threads dequeue and run fibers
- 1 sleep manager thread handles timed wakeups
- Shutdown signaled via TVar Bool

Future Enhancements:
- Per-worker local queues for cache locality
- Work-stealing when local queue empty
- Fiber affinity for socket operations
-}

-- | Thread-safe scheduler state for M:N scheduling
data ParSchedulerState = ParSchedulerState
  { parFibers :: TVar (IntMap FiberEntry)
    -- ^ All fiber entries by ID (thread-safe)
  , parReadyQueue :: TVar (Seq FiberId)
    -- ^ Global ready queue (work-stealing source)
  , parSleeping :: TVar [(Int, FiberId, World -> IO (Either EvalError IOStep))]
    -- ^ Sleeping fibers (one thread manages this)
  , parNextId :: TVar FiberId
    -- ^ Next fiber ID to allocate
  , parResults :: TVar (IntMap Value)
    -- ^ Completed fiber results
  , parAwaitConts :: TVar (IntMap (World -> Value -> IO (Either EvalError IOStep)))
    -- ^ Stored await continuations
  , parShutdown :: TVar Bool
    -- ^ Signal to stop worker threads
  , parMainDone :: TMVar (Either EvalError (World, Value))
    -- ^ Result of main fiber (fiber 0)
  }

-- | Create a new parallel scheduler
newParScheduler :: IO ParSchedulerState
newParScheduler = atomically $ do
  fibers <- newTVar IntMap.empty
  ready <- newTVar Seq.empty
  sleeping <- newTVar []
  nextId <- newTVar 0
  results <- newTVar IntMap.empty
  awaitConts <- newTVar IntMap.empty
  shutdown <- newTVar False
  mainDone <- newEmptyTMVar
  pure ParSchedulerState
    { parFibers = fibers
    , parReadyQueue = ready
    , parSleeping = sleeping
    , parNextId = nextId
    , parResults = results
    , parAwaitConts = awaitConts
    , parShutdown = shutdown
    , parMainDone = mainDone
    }

-- | Spawn a fiber in the parallel scheduler
spawnFiberPar :: ParSchedulerState -> (World -> IO (Either EvalError IOStep)) -> IO FiberId
spawnFiberPar sched cont = atomically $ do
  fid <- readTVar (parNextId sched)
  writeTVar (parNextId sched) (fid + 1)
  let entry = FiberEntry cont []
  modifyTVar' (parFibers sched) (IntMap.insert fid entry)
  modifyTVar' (parReadyQueue sched) (Seq.|> fid)
  pure fid

-- | Create a new scheduler
newScheduler :: IO SchedulerState
newScheduler = do
  fibers <- newIORef IntMap.empty
  ready <- newIORef Seq.empty
  sleeping <- newIORef []
  nextId <- newIORef 0
  results <- newIORef IntMap.empty
  awaitConts <- newIORef IntMap.empty
  mainDone <- newIORef Nothing
  pure SchedulerState
    { schedFibers = fibers
    , schedReadyQueue = ready
    , schedSleeping = sleeping
    , schedNextId = nextId
    , schedResults = results
    , schedAwaitConts = awaitConts
    , schedMainDone = mainDone
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
      -- If the main fiber has completed, stop the scheduler (detaching any
      -- remaining fibers).
      mMain <- readIORef (schedMainDone sched)
      case mMain of
        Just res -> pure (Right res)
        Nothing -> do
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
      when (fid == 0) $
        writeIORef (schedMainDone sched) (Just (world, value))
      fibers <- readIORef (schedFibers sched)
      case IntMap.lookup fid fibers of
        Nothing -> pure ()
        Just entry -> do
          -- Wake all waiters, updating their continuation with the result
          forM_ (fiberWaiters entry) $ \waiterId -> do
            waitConts <- readIORef (schedAwaitConts sched)
            case IntMap.lookup waiterId waitConts of
              Just cont -> do
                -- Update waiter's continuation with the actual result value
                modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) waiterId)
                modifyIORef' (schedAwaitConts sched) (IntMap.delete waiterId)
                modifyIORef' (schedReadyQueue sched) (Seq.|> waiterId)
              Nothing -> pure ()  -- No stored continuation (e.g. already resumed via awaitAny)
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
          -- Add self to target's waiters list and store the await continuation
          modifyIORef' (schedFibers sched) $ \fibers ->
            IntMap.adjust (\e -> e { fiberWaiters = fid : fiberWaiters e }) targetFid fibers
          modifyIORef' (schedAwaitConts sched) (IntMap.insert fid cont)
          -- Fiber stays out of ready queue until target completes
          pure world

    StepAwaitAny world targetFids cont -> do
      results <- readIORef (schedResults sched)
      case firstCompleted results targetFids of
        Just value -> do
          modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) fid)
          modifyIORef' (schedReadyQueue sched) (Seq.|> fid)
          pure world
        Nothing -> do
          modifyIORef' (schedFibers sched) $ \fibers ->
            foldr
              (\targetId -> IntMap.adjust (\e -> e { fiberWaiters = fid : fiberWaiters e }) targetId)
              fibers
              targetFids
          modifyIORef' (schedAwaitConts sched) (IntMap.insert fid cont)
          pure world

    StepSpawn world newCont resumeCont -> do
      -- Spawn new fiber
      newFid <- spawnFiber sched newCont
      -- Resume spawner with the new fiber ID
      modifyIORef' (schedFibers sched) (IntMap.adjust (\e -> e { fiberCont = \w -> resumeCont w newFid }) fid)
      modifyIORef' (schedReadyQueue sched) (Seq.|> fid)
      pure world

firstCompleted :: IntMap Value -> [FiberId] -> Maybe Value
firstCompleted results fids =
  case fids of
    [] -> Nothing
    fid : rest ->
      case IntMap.lookup fid results of
        Just v -> Just v
        Nothing -> firstCompleted results rest

-- | Handle a step result in parallel scheduler
--
-- NOTE: Socket/TLS operations complete within a single step, so thread
-- affinity is not an issue. Each socket accept/read/write returns StepDone
-- atomically. Fibers can migrate between OS threads between steps safely.
handleStepPar :: ParSchedulerState -> FiberId -> IOStep -> IO ()
handleStepPar sched fid step =
  case step of
    StepDone world value -> do
      -- Fiber completed
      atomically $ do
        -- Store result
        modifyTVar' (parResults sched) (IntMap.insert fid value)
        -- Get waiters
        fibers <- readTVar (parFibers sched)
        case IntMap.lookup fid fibers of
          Nothing -> pure ()
          Just entry -> do
            -- Wake all waiters
            forM_ (fiberWaiters entry) $ \waiterId -> do
              awaitConts <- readTVar (parAwaitConts sched)
              case IntMap.lookup waiterId awaitConts of
                Just cont -> do
                  modifyTVar' (parFibers sched) $
                    IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) waiterId
                  modifyTVar' (parAwaitConts sched) (IntMap.delete waiterId)
                  modifyTVar' (parReadyQueue sched) (Seq.|> waiterId)
                Nothing -> pure () -- No stored continuation (e.g. already resumed via awaitAny)
        -- Remove completed fiber
        modifyTVar' (parFibers sched) (IntMap.delete fid)
        -- If main fiber, signal completion
        when (fid == 0) $
          void $ tryPutTMVar (parMainDone sched) (Right (world, value))

    StepYield _world cont -> do
      atomically $ do
        modifyTVar' (parFibers sched) $
          IntMap.adjust (\e -> e { fiberCont = cont }) fid
        modifyTVar' (parReadyQueue sched) (Seq.|> fid)

    StepSleep _world ms cont -> do
      now <- nowMs
      let wakeAt = now + ms
      atomically $ do
        modifyTVar' (parSleeping sched) $
          insertSorted wakeAt fid cont
        modifyTVar' (parFibers sched) $
          IntMap.adjust (\e -> e { fiberCont = cont }) fid

    StepAwait _world targetFid cont -> do
      atomically $ do
        results <- readTVar (parResults sched)
        case IntMap.lookup targetFid results of
          Just value -> do
            -- Already done
            modifyTVar' (parFibers sched) $
              IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) fid
            modifyTVar' (parReadyQueue sched) (Seq.|> fid)
          Nothing -> do
            -- Add to waiters
            modifyTVar' (parFibers sched) $
              IntMap.adjust (\e -> e { fiberWaiters = fid : fiberWaiters e }) targetFid
            modifyTVar' (parAwaitConts sched) (IntMap.insert fid cont)

    StepAwaitAny _world targetFids cont -> do
      atomically $ do
        results <- readTVar (parResults sched)
        case firstCompleted results targetFids of
          Just value -> do
            modifyTVar' (parFibers sched) $
              IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) fid
            modifyTVar' (parReadyQueue sched) (Seq.|> fid)
          Nothing -> do
            modifyTVar' (parFibers sched) $ \fibers ->
              foldr
                (\targetId -> IntMap.adjust (\e -> e { fiberWaiters = fid : fiberWaiters e }) targetId)
                fibers
                targetFids
            modifyTVar' (parAwaitConts sched) (IntMap.insert fid cont)

    StepSpawn _world newCont resumeCont -> do
      atomically $ do
        -- Spawn new fiber atomically with parent update
        newFid <- readTVar (parNextId sched)
        writeTVar (parNextId sched) (newFid + 1)
        let entry = FiberEntry newCont []
        modifyTVar' (parFibers sched) (IntMap.insert newFid entry)
        modifyTVar' (parReadyQueue sched) (Seq.|> newFid)
        -- Update parent
        modifyTVar' (parFibers sched) $
          IntMap.adjust (\e -> e { fiberCont = \w -> resumeCont w newFid }) fid
        modifyTVar' (parReadyQueue sched) (Seq.|> fid)

-- | Try to dequeue a ready fiber (STM transaction)
tryDequeue :: ParSchedulerState -> STM (Maybe FiberId)
tryDequeue sched = do
  q <- readTVar (parReadyQueue sched)
  case Seq.viewl q of
    Seq.EmptyL -> pure Nothing
    fid Seq.:< rest -> do
      writeTVar (parReadyQueue sched) rest
      pure (Just fid)

-- | Sleep manager thread - wakes sleeping fibers at their scheduled time
sleepManager :: ParSchedulerState -> IO ()
sleepManager sched = loop
  where
    loop = do
      shouldStop <- atomically $ readTVar (parShutdown sched)
      if shouldStop
        then pure ()
        else do
          wakeSleptPar sched
          -- Sleep until next scheduled wakeup or 10ms
          nextWake <- atomically $ do
            sleeping <- readTVar (parSleeping sched)
            case sleeping of
              [] -> pure Nothing
              ((wake, _, _) : _) -> pure (Just wake)
          now <- nowMs
          case nextWake of
            Nothing -> threadDelay 10000  -- 10ms if no sleepers
            Just wake -> do
              let delay = max 0 (wake - now)
              threadDelay (delay * 1000)
          loop

-- | Wake sleeping fibers that are due (parallel version)
wakeSleptPar :: ParSchedulerState -> IO ()
wakeSleptPar sched = do
  now <- nowMs
  atomically $ do
    sleeping <- readTVar (parSleeping sched)
    let (due, remaining) = span (\(wake, _, _) -> wake <= now) sleeping
    writeTVar (parSleeping sched) remaining
    forM_ due $ \(_, fid, cont) -> do
      modifyTVar' (parFibers sched) $
        IntMap.adjust (\e -> e { fiberCont = cont }) fid
      modifyTVar' (parReadyQueue sched) (Seq.|> fid)

-- | Run the parallel scheduler with N worker threads
-- N is determined by GHC runtime capabilities
runParScheduler :: ParSchedulerState -> World -> IO (Either EvalError (World, Value))
runParScheduler sched world0 = do
  -- Get number of capabilities (OS threads GHC can use)
  numCaps <- getNumCapabilities
  let numWorkers = max 1 (numCaps - 1)  -- Reserve one for sleep manager

  -- Store initial World in a TVar for workers to clone
  -- (Each fiber step gets a fresh copy)
  worldVar <- newTVarIO world0

  -- Start sleep manager
  _ <- forkIO $ sleepManager sched

  -- Start worker threads
  replicateM_ numWorkers $ forkIO $ workerThreadWithWorld sched worldVar

  -- Wait for main fiber to complete
  atomically $ do
    result <- takeTMVar (parMainDone sched)
    -- Signal shutdown
    writeTVar (parShutdown sched) True
    pure result

-- | Worker thread with access to World template
workerThreadWithWorld :: ParSchedulerState -> TVar World -> IO ()
workerThreadWithWorld sched worldVar = loop
  where
    loop = do
      shouldStop <- atomically $ readTVar (parShutdown sched)
      if shouldStop
        then pure ()
        else do
          mFid <- atomically $ tryDequeue sched
          case mFid of
            Nothing -> do
              threadDelay 1000
              loop
            Just fid -> do
              mEntry <- atomically $ IntMap.lookup fid <$> readTVar (parFibers sched)
              case mEntry of
                Nothing -> loop
                Just entry -> do
                  -- Get a copy of World for this fiber step
                  world <- readTVarIO worldVar
                  result <- fiberCont entry world
                  case result of
                    Left err -> do
                      when (fid == 0) $
                        atomically $ void $ tryPutTMVar (parMainDone sched) (Left err)
                      loop
                    Right step -> do
                      handleStepPar sched fid step
                      loop

-- Helper to insert into sorted sleeping list
insertSorted :: Int -> FiberId -> (World -> IO (Either EvalError IOStep)) -> [(Int, FiberId, World -> IO (Either EvalError IOStep))] -> [(Int, FiberId, World -> IO (Either EvalError IOStep))]
insertSorted wake fid' cont [] = [(wake, fid', cont)]
insertSorted wake fid' cont (x@(w, _, _) : xs)
  | wake <= w = (wake, fid', cont) : x : xs
  | otherwise = x : insertSorted wake fid' cont xs
