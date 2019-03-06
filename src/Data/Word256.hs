module Data.Word256
  ( -- * The @Word256@ type
    Word256(..)
    -- Operations
  , fromInt
  , toInt
  , toHex
  , significant
  , size
    -- Constants
  , bytes
  ) where

import Text.Show.Extra (toBytes)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import qualified Data.ByteString.Base16.Extra as Base16
import Data.List.Extra (padLeft)

-- | Represents a 32-byte word.
newtype Word256 = Word256 [Word8]
  deriving (Show, Eq)

instance ToText Word256 where
  toText = toHex

-- | Converts `Word256` to `Int`.
toInt :: Word256 -> Int
toInt (Word256 bs) =
  fromMaybe (error $ "Invalid word: " <> show bs) (decode bs)
  where
    decode :: [Word8] -> Maybe Int
    decode = readMaybe
      . Char8.unpack
      . ByteString.pack
      . significantBytes

-- | Converts `Int` to `Word256`.
fromInt :: Int -> Word256
fromInt = Word256 . pad . toBytes

-- | Returns number of significant bytes in word.
size :: Word256 -> Int
size = length . significant

-- | Pads list from the left with `0`-bytes.
pad :: [Word8] -> [Word8]
pad = padLeft bytes 0

-- | Converts `Word8` to base16-encoded `Text`.
toHex :: Word256 -> Text
toHex = toText . Base16.encodeBytes . coerce

-- | Retuns only significant (non-zero) bytes.
significant :: Word256 -> [Word8]
significant = significantBytes . coerce

significantBytes :: [Word8] -> [Word8]
significantBytes = dropWhile (== 0)

-- | `Word256` size in bytes.
bytes :: Int
bytes = 32
