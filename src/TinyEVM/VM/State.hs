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

import Prelude hiding (State, show, state)

import Control.Lens (makeLenses, (^.))
import qualified Data.String as String
import Text.Show (show)

import TinyEVM.VM.Memory (Memory, newMemory)
import qualified TinyEVM.VM.Memory as Memory
import TinyEVM.VM.Stack (Stack)
import qualified TinyEVM.VM.Stack as Stack
import TinyEVM.VM.Storage (Storage)
import qualified TinyEVM.VM.Storage as Storage

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

instance Show State where
  show s = String.unlines
    [ "VM State:"
    , "pc: " <> show (s ^. pc)
    , "gas: " <> show (s ^. gas)
    , "stack:"
    , show (Stack.toList $ s ^. stack)
    , "memory:"
    , " ... "
    , "storage:"
    , show (Storage.toList $ s ^. storage)
    ]

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
