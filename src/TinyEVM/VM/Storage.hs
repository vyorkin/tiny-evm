{-# LANGUAGE LambdaCase #-}

module TinyEVM.VM.Storage
  ( -- * The @Storage@ type
    Storage(..)
  , Address
  , Value
    -- * Opreations
  , fromList
  , toList
  , empty
  , get
  , put
  ) where

import Prelude hiding (empty, get, put, fromList, toList)

import Data.Aeson (FromJSON, parseJSON, withObject, withText)
import qualified Data.Aeson as Aeson (Value (..))
import Data.Aeson.Types (Object, Parser)
import qualified Data.ByteString.Base16.Extra as Base16
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- In the real EVM we have a storage per contract account.
-- But for our TinyEVM (to keep things simple) we'll have a
-- storage per program, which is just an abstraction over `HashMap`.

type Address = Word8

type Value = Word8

-- | Represents a VM storage.
newtype Storage = Storage
  { unStorage :: HashMap Address Value
  } deriving (Eq, Show)

instance FromJSON Storage where
  parseJSON = withObject "storage" parseStorage

-- | Creates a new empty storage.
empty :: Storage
empty = Storage HashMap.empty

fromList :: [(Address, Value)] -> Storage
fromList = Storage . HashMap.fromList

toList :: Storage -> [(Address, Value)] 
toList = HashMap.toList . unStorage

parseByteValue :: Aeson.Value -> Parser Value
parseByteValue = withText "byte" parseByteText

parseByteText :: Text -> Parser Value
parseByteText s = maybe err return (Base16.decodeByte s)
  where
    err = fail $ "Invalid byte: " <> show s

parseStorage :: Object -> Parser Storage
parseStorage = fmap fromList
  . traverse (bitraverse parseByteText parseByteValue)
  . HashMap.toList

-- | Return the value to which the specified key is mapped,
-- or `Nothing` if this map contains no value for the address (key).
get :: Storage -> Address -> Maybe Value
get (Storage h) k = HashMap.lookup k h

-- | Puts value into the storage under the given address (key).
put :: Storage -> Address -> Value -> Storage
put (Storage h) k = Storage . update
  where
    update :: Value -> HashMap Address Value
    update 0 = HashMap.delete k h
    update v = HashMap.insert k v h
