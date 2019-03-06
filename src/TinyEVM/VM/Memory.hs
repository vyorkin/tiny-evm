{-# LANGUAGE RecordWildCards #-}

module TinyEVM.VM.Memory
  ( -- * The @Memory@ type
    Memory(..)
  , MemoryError(..)
    -- * Operations
  , newMemory
  , fromIOVector
  , read
  , readWord
  , write
  , writeWord
  , toByteString
  , toHex
    -- * Constants
  , maxOffset
  ) where

import Prelude

import qualified Data.ByteString as ByteString
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as IOVector
import Text.Show (Show)

import qualified Data.ByteString.Base16.Extra as Base16
import Data.List.Extra (padRight)
import Data.Word256 (Word256(..))
import qualified Data.Word256 as Word256

-- | Represents a VM memory.
data Memory = Memory
  { vec :: IOVector Word8 -- ^ Internal representation of the VM memory.
  , pos :: IORef Int      -- ^ Index of the highest memory location used.
  }

-- | Represents an error that
-- might occur when working with `Memory`.
data MemoryError = OutOfMemory (Int, Word256)
  deriving (Eq, Show)

instance ToText MemoryError where
  toText (OutOfMemory (offset, word)) = unlines
    [ "Out of memory when attempting to write at offset "
    , show offset
    , " word "
    , show word
    ]

-- | Creates a new empty `Memory` given the initial capacity (in words).
newMemory :: Int -> IO Memory
newMemory capacity = do
  vec <- IOVector.new $ capacity * Word256.bytes
  pos <- newIORef 0
  return Memory {..}

-- | Creates a new `Memory` from the given `IOVector`.
fromIOVector :: IOVector Word8 -> IO Memory
fromIOVector vec = do
  let len = IOVector.length vec
  pos <- newIORef len
  return Memory {..}

-- | Reads a value at the given offset.
read :: Memory -> Int -> IO Int
read mem offset = Word256.toInt <$> readWord mem offset

-- | Reads a "word" out of memory starting from the given offset.
-- Returns a word of `0`-bytes if the offset is too high.
readWord :: Memory -> Int -> IO Word256
readWord mem o = Word256 <$> readBytes mem o Word256.bytes

-- | Reads the given number of bytes out of memory starting from the given offset.
-- Returns a list of `0`-bytes if the offset is too high.
readBytes :: Memory -> Int -> Int -> IO [Word8]
readBytes mem o n = traverse (readByte $ vec mem) (range o n)

-- | Reads a single byte out of the vector at the given offset.
-- Returns `0` if the offset is too high (for convenience).
readByte :: IOVector Word8 -> Int -> IO Word8
readByte v o
  | o < IOVector.length v = IOVector.read v o
  | otherwise             = return 0

-- | Writes value at the given offset.
write :: Memory -> Int -> Int -> IO (Either MemoryError Memory)
write mem offset val = writeWord mem offset (Word256.fromInt val)

-- | Writes a `Word256` at the given offset.
writeWord :: Memory -> Int -> Word256 -> IO (Either MemoryError Memory)
writeWord mem offset word = do
  size <- readIORef (pos mem)
  let
    needed = offset + Word256.bytes
    len = IOVector.length (vec mem)
  return $ Right mem

-- | Converts `Memory` to a base16 text prefixed by the `"0x"`.
toHex :: Memory -> IO Text
toHex mem = Base16.encode <$> toByteString mem

-- | Converts `Memory` to a `ByteString`.
toByteString :: Memory -> IO ByteString
toByteString mem = do
  size <- readIORef (pos mem)
  ByteString.pack <$> readBytes mem 0 size

-- | Capacity increase factor.
capacityFactor :: Float
capacityFactor = 0.5

-- | Maximum allowed offset (128 words).
maxOffset :: Int
maxOffset = 128 * Word256.bytes

wordRange :: Int -> [Int]
wordRange = flip range Word256.bytes

range :: Int -> Int -> [Int]
range i n = [i..i + n - 1]
