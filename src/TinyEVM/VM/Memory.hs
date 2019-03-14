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

import Prelude hiding (words)

import qualified Data.ByteString as ByteString
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as IOVector
import Data.Vector.Unboxed.Mutable.Extra (readBytes, writeBytes)

import qualified Data.ByteString.Base16.Extra as Base16
import Data.Word256 (Word256 (..))
import qualified Data.Word256 as Word256

-- | Represents a (mutable) VM memory.
data Memory = Memory
  { vector :: IOVector Word8 -- ^ Internal representation of the VM memory.
  , wordIx :: IORef Int      -- ^ Index of the highest word.
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
  vector <- IOVector.new $ capacity * Word256.bytes
  wordIx <- newIORef 0
  return Memory {..}

-- | Creates a new `Memory` from the given `IOVector`.
fromIOVector :: IOVector Word8 -> IO Memory
fromIOVector vector = do
  let len = IOVector.length vector
  wordIx <- newIORef $ len `div` Word256.bytes
  return Memory {..}

-- | Reads a value at the given offset.
read :: Memory -> Int -> IO Int
read mem offset = Word256.toInt <$> readWord mem offset

-- | Reads a "word" out of memory starting from the given offset.
  -- Returns a word of `0`-bytes if the offset is too high.
readWord :: Memory -> Int -> IO Word256
readWord mem o = Word256 <$> readBytes (vector mem) o Word256.bytes

-- | Writes at the given offset.
write :: Memory -> Int -> Int -> IO (Either MemoryError Memory)
write mem offset val = writeWord mem offset (Word256.fromInt val)

-- | Writes a `Word256` at the given offset.
writeWord :: Memory -> Int -> Word256 -> IO (Either MemoryError Memory)
writeWord mem offset word
  | offset > maxOffset = return $ Left $ OutOfMemory (offset, word)
  | otherwise = do
      mem' <- expand mem (offset + Word256.bytes)
      writeBytes (vector mem') offset (Word256.toBytes word)
      return $ Right mem'

-- | Expands the memory capacity, if needed.
expand :: Memory -> Int -> IO Memory
expand mem 0 = return mem
expand mem needed = do
  let
    capacity = IOVector.length (vector mem)
    remaining = max 0 (needed - capacity)
  mem' <- grow mem remaining
  let words' = needed `div` Word256.bytes + 1
  writeIORef (wordIx mem') words'
  return mem'

-- | Grows a memory by the given number of bytes.
grow :: Memory -> Int -> IO Memory
grow (Memory v i) k = do
  let n = k + Word256.bytes * capacityFactor
  v' <- IOVector.grow v n
  return $ Memory v' i

-- | Converts `Memory` to a base16 text prefixed by the `"0x"`.
toHex :: Memory -> IO Text
toHex mem = Base16.encode <$> toByteString mem

-- | Converts `Memory` to a `ByteString`.
toByteString :: Memory -> IO ByteString
toByteString mem = do
  words <- readIORef (wordIx mem)
  ByteString.pack <$> readBytes (vector mem) 0 (words * Word256.bytes)

-- | Capacity increase factor (in words).
capacityFactor :: Int
capacityFactor = 10

-- | Maximum allowed offset (128 words).
maxOffset :: Int
maxOffset = 128 * Word256.bytes
