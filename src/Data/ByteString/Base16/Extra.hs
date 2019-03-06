module Data.ByteString.Base16.Extra
  ( decode
  , decodeBytes
  , decodeByte
  , encode
  , encodeBytes
  ) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import Data.Text (stripPrefix)

decodeBytes :: Text -> [Word8]
decodeBytes = ByteString.unpack . decode

-- | Attempts to decode the first
-- byte from the given base16-encoded `Text`.
decodeByte :: Text -> Maybe Word8
decodeByte = listToMaybe . decodeBytes

-- | Attempts to decode the given base16-encoded `Text`.
decode :: Text -> ByteString
decode = fst . Base16.decode . encodeUtf8 . strip0x

-- | Encodes the given `ByteString` as
-- a base16 text prefixed by the `"0x"`.
encodeBytes :: [Word8] -> Text
encodeBytes = encode . ByteString.pack

-- | Encodes the given `ByteString` as
-- a base16 text prefixed by the `"0x"`.
encode :: ByteString -> Text
encode = ("0x" <>) . decodeUtf8 . Base16.encode

-- | Drops the "0x" prefix from the given `Text`.
-- It returns the given `Text` as is if it did not start with the "0x".
strip0x :: Text -> Text
strip0x s = fromMaybe s $ stripPrefix "0x" s
