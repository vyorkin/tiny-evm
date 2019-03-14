{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module TinyEVM.VM
  ( VM (..)
  , runVM
  , MonadVM
  , module TinyEVM.VM.State
  ) where

import Prelude hiding (div, mod)

import Control.Natural (type (~>))
import Control.Lens ((^.))
import Colog (Message, WithLog)

import TinyEVM.Env (Env (..))
import TinyEVM.VM.State (State)
import TinyEVM.VM.Instruction (Operation (..), Metadata (..), Instruction, operation, opcode, metadata, operands)
import TinyEVM.VM.Instruction.Metadata (inputs, outputs)

newtype VM a = VM
  { unVM :: ReaderT (Env VM) IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Env VM)
    )

runVM :: Env VM -> VM ~> IO
runVM env = usingReaderT env . unVM

type MonadVM m =
  ( WithLog (Env VM) Message m
  , MonadIO m
  )

-- | Runs the given VM instruction.
run :: MonadVM m => Instruction -> m ()
run i = do
  exec (i ^. operation) (i ^. operands)

-- | Executes the operation given its arguments.
exec :: MonadVM m => Operation -> [Word8] -> m ()
exec Stop _      = return ()
exec Add (x:y:_) = return ()
exec (Push n) _  = return ()
