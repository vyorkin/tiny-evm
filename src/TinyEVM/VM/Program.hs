module TinyEVM.VM.Program
  ( -- * The @Program@ type
    Program(..)
  ) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

import TinyEVM.VM.Code (Code (..))
import TinyEVM.VM.Storage (Storage)

-- | Represents a TinyEVM program.
-- Basically, this is a code, initial gas and a storage.
data Program = Program Code Int Storage
  deriving (Eq, Show)

instance FromJSON Program where
  parseJSON = withObject "Program" $ \o ->
    Program <$> o .: "code"
            <*> o .: "gas"
            <*> o .: "storage"
