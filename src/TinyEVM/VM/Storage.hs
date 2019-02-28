module TinyEVM.VM.Storage
  ( -- * The @Storage@ type
    Storage
  , empty
  ) where

import Prelude hiding (empty)

data Storage = Storage

empty :: Storage
empty = Storage
