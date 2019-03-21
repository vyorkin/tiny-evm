{-# LANGUAGE TemplateHaskell #-}
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

import Prelude hiding (mod)

import Control.Natural (type (~>))
import Control.Lens ((^.), view)
import Colog (Message, WithLog)

import TinyEVM.Env (Env (..), vm, trace)
import TinyEVM.VM.State (State, stack, memory, storage)
import TinyEVM.VM.Instruction (Operation (..), Metadata (..), Instruction, operation, opcode, metadata, operands)
import TinyEVM.VM.Instruction.Metadata (inputs, outputs)
import qualified TinyEVM.VM.Storage as Storage
import qualified TinyEVM.VM.Memory as Memory
import qualified TinyEVM.VM.Stack as Stack

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

-- | Synonym for constraints commonly
-- satisfied by monads used in stack.
type MonadVM m =
  ( WithLog (Env VM) Message m
  , MonadIO m
  )

-- | Runs the given VM instruction.
run :: MonadVM m => Instruction -> m ()
run i = do
  exec (i ^. operation) (i ^. operands)


-- | Executes the operation given its arguments.
--
-- Runs a single execution cycle returning the new state,
-- defined as `O` in the Yellow Paper, Eq.(143).
exec :: MonadVM m => Operation -> [Word8] -> m ()
exec Stop _ = return () -- Normal halting
exec (Push n) xs = return ()
exec op _ = return ()
-- exec op args = do
--   state <- readIORef <$> asks (^. (vm . stack))
--   r <- execPure op args
--   writeIORef

execPure :: Operation -> [Word8] -> Word8
execPure Add (x:y:_) = x + y
execPure Mul (x:y:_) = x * y
execPure Sub (x:y:_) = x - y
execPure Div (_:0:_) = 0
execPure Div (x:y:_) = x `div` y
execPure AddMod (_:_:0:_) = 0
execPure AddMod (x:y:z:_) = (x + y) `rem` z
execPure MulMod (_:_:0:_) = 0
execPure MulMod (x:y:z:_) = (x * y) `rem` z
execPure Mod (_:0:_) = 0
execPure Mod (x:y:_) = x `rem` y
execPure op args = error $ "Invalid operation " <> show op <> ", args: " <> show args
