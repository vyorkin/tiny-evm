module TinyEVM.VM.Exception
  ( StackException(..)
  , InvalidOpcodeException(..)
  , InsufficientGasException(..)
  , UnsupportedOperationException(..)
  ) where

import Prelude hiding (show)

import Text.Show (show)
import qualified Data.Text as Text

import TinyEVM.VM.Instruction (Operation (..), InvalidOpcode)
import TinyEVM.VM.Stack (StackError(..))

-- | Exception that might occur in case of stack overflow/underflow.
newtype StackException = StackException StackError

instance Exception StackException
instance Show StackException where
  show (StackException e) = Text.unpack $ toText e

-- | Exception that might occur when
-- the VM encounters unknown/invalid opcode.
newtype InvalidOpcodeException =
  InvalidOpcodeException InvalidOpcode

instance Exception InvalidOpcodeException
instance Show InvalidOpcodeException where
  show (InvalidOpcodeException e) = Text.unpack $ toText e

-- | Exception that might occur when there is no gas left. 
data InsufficientGasException =
  InsufficientGasException Operation (Int, Int)

instance Exception InsufficientGasException
instance Show InsufficientGasException where
  show (InsufficientGasException op (actual, needed)) =
    "Not enough gas to execute the " <> show op <>
    " operation. The actual gas is " <> show actual <>
    " but needed " <> show needed

-- | Exception that might occur when
-- the VM encounters unsupported operation.
newtype UnsupportedOperationException =
  UnsupportedOperationException (Operation, [Word8])

instance Exception UnsupportedOperationException
instance Show UnsupportedOperationException where
  show (UnsupportedOperationException (op, args)) =
    "Unsupported operation: " <> show op <> " (" <> show args <> ")"
