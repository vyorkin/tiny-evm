module TinyEVM.VM.Program
  ( -- * The @Program@ type
    Program(..)
  , Code(..)
    -- * Operations
  ) where

import Data.Aeson (FromJSON, ToJSON, Value (..), parseJSON, withObject, (.:))

import qualified Data.ByteString.Base16.Extra as Base16
import TinyEVM.VM.Code (Code (..))
import TinyEVM.VM.Instruction (Instruction)
import TinyEVM.VM.Storage (Storage)
import qualified TinyEVM.VM.Storage as Storage

-- | Represents a TinyEVM program.
-- Basically, this is a code, initial gas and a storage.
data Program = Program Code Integer Storage
  deriving (Eq, Show)

instance FromJSON Program where
  parseJSON = withObject "Program" $ \o ->
    Program <$> o .: "code"
            <*> o .: "gas"
            <*> o .: "storage"
