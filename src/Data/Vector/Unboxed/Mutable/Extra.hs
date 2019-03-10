module Data.Vector.Unboxed.Mutable.Extra
  ( readBytes
  , readByte
  , writeBytes
  ) where

import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as IOVector

-- | Given offset and a number of
-- bytes reads those bytes out of `IOVector Word8`.
-- Returns a list of `0`-bytes if the offset is too high.
readBytes :: IOVector Word8 -> Int -> Int -> IO [Word8]
readBytes vec o n = traverse (readByte vec) (range o n)

-- | Reads a single byte out of the vector at the given offset.
-- Returns `0` if the offset is too high (for convenience).
readByte :: IOVector Word8 -> Int -> IO Word8
readByte v o
  | o < IOVector.length v = IOVector.read v o
  | otherwise             = return 0

-- | Writes the given list of bytes starting from the specified offset.
writeBytes :: IOVector Word8 -> Int -> [Word8] -> IO ()
writeBytes v o bs = traverse_ (uncurry $ IOVector.write v) (zip [o..] bs)

range :: Int -> Int -> [Int]
range i n = [i..i + n - 1]
