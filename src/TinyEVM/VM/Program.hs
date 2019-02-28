module TinyEVM.VM.Program
  ( -- * The @Program@ type
    Program(..)
  , Code(..)
    -- * Operations
  ) where

import Data.Aeson (FromJSON, ToJSON, Value (..), parseJSON, toJSON)

import TinyEVM.VM.Code (Code (..))
import TinyEVM.VM.Instruction (Instruction)
import TinyEVM.VM.Storage (Storage)
import qualified TinyEVM.VM.Storage as Storage

-- | Represents a TinyEVM program.
-- Basically, this is the code, initial gas and a storage.
data Program = Program Code Integer Storage

instance FromJSON Program where
  parseJSON (Object _) = pure $ Program (Code []) 0 Storage.empty
  parseJSON _          = mzero
