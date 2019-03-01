module TinyEVM.VM.Storage
  ( -- * The @Storage@ type
    Storage(..)
  , parseByteValue
  ) where

import Prelude hiding (empty)

import Data.Aeson (FromJSON, Value, parseJSON, withObject, withText)
import Data.Aeson.Types (Object, Parser)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified Data.ByteString.Base16.Extra as Base16

-- In the real EVM we have a storage per contract account.
-- But for our TinyEVM (to keep things simple) we'll have a
-- storage per program, which is just an abstraction over `HashMap`.

-- | Represents a VM storage.
newtype Storage = Storage
  { unStorage :: HashMap Word8 Word8
  } deriving (Eq, Show)

instance FromJSON Storage where
  parseJSON = withObject "storage" parseStorage

parseByteValue :: Value -> Parser Word8
parseByteValue = withText "byte" parseByte

parseByte :: Text -> Parser Word8
parseByte s = maybe err return (Base16.decodeByte s)
  where
    err :: Parser Word8
    err = fail $ "Invalid byte: " ++ show s

parseStorage :: Object -> Parser Storage
parseStorage = fmap (Storage . HashMap.fromList)
  . traverse (bitraverse parseByte parseByteValue)
  . HashMap.toList
