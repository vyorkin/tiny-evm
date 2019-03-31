{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module TinyEVM.Env
  ( Env(..)
  , mkEnv
  , logger
  , code
  , vm
  , trace
  ) where

import Prelude hiding (trace)

import Colog (HasLog, LogAction, Message, cmapM, defaultFieldMap,
              fmtRichMessageDefault, getLogAction, liftLogIO, logTextStdout,
              setLogAction, upgradeMessageAction)
import Control.Lens (makeLenses, view, (.~))
import TinyEVM.VM.Code (Code (..))
import TinyEVM.VM.Program (Program (..))
import qualified TinyEVM.VM.State as VM

-- | Global environment stores read-only information and
-- mutable refs to global state available to any
-- function with the `MonadReader` constraint.
data Env m = Env
  { -- | A `LogAction` to be used by the `co-log` package.
    _logger :: !(LogAction m Message)
    -- | Code to be executed.
  , _code :: !Code
    -- | Reference to a VM state.
  , _vm :: !(IORef VM.State)
    -- | Whether to trace the VM execution.
  , _trace :: !Bool
  }

makeLenses ''Env

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = view logger

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction action env = env & logger .~ action

-- | Creates a record that matches the `Env` type our
-- application requires by filling in necessary fields.
mkEnv :: MonadIO m => LogAction IO Text -> Program -> IO (Env m)
mkEnv logTextFile (Program code' gas' storage') = do
  let
    logText = logTextStdout <> logTextFile
    logRich = cmapM fmtRichMessageDefault logText
    logFull = upgradeMessageAction defaultFieldMap logRich
    logAction = liftLogIO logFull
  vmState <- VM.mkState gas' storage' >>= newIORef
  return $ Env logAction code' vmState True
