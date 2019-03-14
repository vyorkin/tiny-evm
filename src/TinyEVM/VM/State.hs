{-# LANGUAGE TemplateHaskell #-}

module TinyEVM.VM.State
  ( -- * The @State* type
    State
  , mkState
    -- * Lenses
  , pc
  , gas
  , stack
  , memory
  , storage
  ) where

import Prelude hiding (State)

import Control.Lens (makeLenses)

import TinyEVM.VM.Memory (Memory, newMemory)
import TinyEVM.VM.Stack (Stack)
import qualified TinyEVM.VM.Stack as Stack
import TinyEVM.VM.Storage (Storage)

-- | State of the virtual machine.
-- This is equivalent to the µ in the YP.
data State = State
  { _pc      :: !Int
  , _gas     :: !Int
  , _stack   :: !Stack
  , _memory  :: !Memory
  , _storage :: !Storage
  }

makeLenses ''State

-- | Creates a new VM state with
-- the given amount of gas and storage.
mkState :: Int -> Storage -> IO State
mkState gas' storage' = do
  mem <- newMemory 8
  return State
    { _pc      = 0
    , _gas     = gas'
    , _stack   = Stack.empty
    , _memory  = mem
    , _storage = storage'
    }
