module TinyEVM.VM.Memory
  ( -- * The @Memory@ type
    Memory(..)
  , MemoryError(..)
  , empty
    -- * Operations
  , read
  , write
  ) where

import Prelude hiding (Show, empty, read, show)

import Data.Aeson (ToJSON, Value (..), toJSON)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Show (Show, show)

-- | Represents a VM memory.
data Memory = Memory { unMemory :: !(Vector Integer) }
  deriving (Eq, Show)

-- | Represents an error that
-- might occur when working with `Memory`.
data MemoryError = OutOfMemory (Integer, Integer)
  deriving (Eq)

instance Show MemoryError where
  show (OutOfMemory (addr, val)) = "Out of memory"

-- | Reads a word out of `memory` from the given `offset`.
read :: Memory -> Integer -> Integer
read mem addr = 0

-- | Writes `data` to `memory` at the given `offset`.
write :: Memory -> (Integer, Integer) -> Either MemoryError Memory
write mem (add, val) = Right mem

empty :: Memory
empty = Memory $ Vector.empty
