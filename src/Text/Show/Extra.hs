module Text.Show.Extra
  ( toBytes
  ) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

-- | Converts the given value to a list of bytes.
toBytes :: Show a => a -> [Word8]
toBytes = ByteString.unpack . Char8.pack . show
