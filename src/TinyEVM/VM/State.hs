{-# LANGUAGE TemplateHaskell #-}

module TinyEVM.VM.State
  ( -- * The @State* type
    State
  , Code
  , mkState
    -- * Lenses
  , code
  , pc
  , gas
  , stack
  , memory
  , storage
  ) where

import Prelude hiding (State)

import Control.Lens (makeLenses)

import TinyEVM.VM.Instruction (Instruction)
import TinyEVM.VM.Memory (Memory, newMemory)
import qualified TinyEVM.VM.Memory as Memory
import TinyEVM.VM.Program (Code, Program (..))
import qualified TinyEVM.VM.Program as Program
import TinyEVM.VM.Stack (Stack)
import qualified TinyEVM.VM.Stack as Stack
import TinyEVM.VM.Storage (Storage)
import qualified TinyEVM.VM.Storage as Storage

-- | State of the virtual machine.
-- This is equivalent to the µ in the YP.
data State = State
  { _code    :: !Code
  , _pc      :: !Int
  , _gas     :: !Int
  , _stack   :: !Stack
  , _memory  :: !Memory
  , _storage :: !Storage
  }

makeLenses ''State

-- | Creates a new VM state from the given program.
mkState :: Program -> IO State
mkState (Program code' gas' storage') = do
  mem <- newMemory 8
  return State
    { _code    = code'
    , _pc      = 0
    , _gas     = gas'
    , _stack   = Stack.empty
    , _memory  = mem
    , _storage = storage'
    }
