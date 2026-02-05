{-# LANGUAGE ForeignFunctionInterface #-}

module Lune.Eval.FFI
  ( ffiCall
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import Lune.Eval.Types (Value(..), EvalError(..))

-- | Import C puts function directly via Haskell FFI
foreign import ccall "puts" c_puts :: CString -> IO CInt

-- | Import C strlen function
foreign import ccall "strlen" c_strlen :: CString -> IO CInt

-- | Registry of whitelisted C functions we can call
-- Maps C symbol name to a Haskell implementation
ffiRegistry :: [(Text, [Value] -> IO (Either EvalError Value))]
ffiRegistry =
  [ ("puts", ffi_puts)
  , ("strlen", ffi_strlen)
  ]

-- | Main FFI call dispatcher
ffiCall :: Text -> [Value] -> IO (Either EvalError Value)
ffiCall symbol args =
  case lookup symbol ffiRegistry of
    Nothing -> pure $ Left $ ForeignError $ "Unknown FFI symbol: " <> symbol
    Just impl -> impl args

-- | Implementation: puts(const char* str) -> int
ffi_puts :: [Value] -> IO (Either EvalError Value)
ffi_puts [VString s] = do
  result <- withCString (T.unpack s) c_puts
  pure $ Right $ VInt $ fromIntegral result
ffi_puts args =
  pure $ Left $ ForeignError $ "puts expects 1 String argument, got: " <> T.pack (show args)

-- | Implementation: strlen(const char* str) -> size_t
ffi_strlen :: [Value] -> IO (Either EvalError Value)
ffi_strlen [VString s] = do
  result <- withCString (T.unpack s) c_strlen
  pure $ Right $ VInt $ fromIntegral result
ffi_strlen args =
  pure $ Left $ ForeignError $ "strlen expects 1 String argument, got: " <> T.pack (show args)
