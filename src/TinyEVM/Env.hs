{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module TinyEVM.Env
  ( Env(..)
  , mkEnv
  , logger
  , vm
  ) where

import Colog (HasLog, LogAction, Message, cmapM, defaultFieldMap,
              fmtRichMessageDefault, getLogAction, liftLogIO, logTextStdout,
              setLogAction, upgradeMessageAction, withLogTextFile)
import Control.Lens (makeLenses, (.~), (^.))
import qualified TinyEVM.VM as VM

-- | Global environment stores read-only information and
-- mutable refs to global state available to any
-- function with the `MonadReader` constraint.
data Env m = Env
  { -- | A `LogAction` to be used by the `co-log` package.
    _logger :: !(LogAction m Message)
  , _vm     :: !VM.State
  }

makeLenses ''Env

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = (^. logger)

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction action env = env & logger .~ action

-- | Creates a record that matches the `Env` type our
-- application requires by filling in necessary fields.
mkEnv :: MonadIO m => LogAction IO Text -> Env m
mkEnv = undefined
-- mkEnv cfg net logTextFile = do
--   let
--     logText   = logTextStdout <> logTextFile
--     logRich   = cmapM fmtRichMessageDefault logText
--     logFull   = upgradeMessageAction defaultFieldMap logRich
--     logAction = liftLogIO logFull
--   Env logAction vm
