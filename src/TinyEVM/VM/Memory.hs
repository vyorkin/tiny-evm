module TinyEVM.VM.Memory
  ( -- * The @Memory@ type
    Memory
  , empty
    -- * Operations
  ) where

import Prelude hiding (empty)

data Memory = Memory

empty :: Memory
empty = Memory
