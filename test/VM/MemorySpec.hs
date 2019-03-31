module VM.MemorySpec (spec_memory) where

import Test.Hspec
import Test.QuickCheck hiding (vector)
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Monadic (assert, monadicIO, run, monitor)

import Control.Exception (try, catch)
import qualified Data.Vector.Unboxed.Mutable as IOVector

import Data.Word256 (Word256)
import Data.Vector.Unboxed.Mutable.Extra (readBytes)
import qualified Data.Word256 as Word256
import TinyEVM.VM.Memory (Memory(..), MemoryException(..), newMemory, maxOffset)
import qualified TinyEVM.VM.Memory as Memory
import Data.Word256Spec ()

newtype Capacity = Capacity Int
  deriving (Show)

instance Arbitrary Capacity where
  arbitrary = Capacity . (+ 1) . getPositive <$> arbitrary

spec_memory :: Spec
spec_memory = do
  describe "TinyEVM.VM.Memory" $ do
    describe "newMemory" $ do
      it "respects the given capacity" $ property $ \(Capacity capacity) ->
        monadicIO $ do
          mem <- run $ newMemory capacity
          let
            len = IOVector.length (vector mem)
            expectedLen = capacity * Word256.bytes
          monitor (counterexample $ show len <> " /= " <> show expectedLen)
          assert $ len == expectedLen

    describe "writeWord" $ do
      context "given a valid offset" $ do
        it "writes correctly" $ property $
          \word (Capacity capacity) (Positive offset) ->
            monadicIO $ do
              let
                size = Word256.size word
                expected = Word256.significant word
              actual <- run $ do
                -- traceShowM $ Word256.toHex word
                mem <- newMemory capacity >>= Memory.writeWord offset word
                -- Memory.toHex mem >>= traceShowM
                let
                  start = offset + Word256.bytes - size
                  slice = IOVector.slice start (size + 1) (vector mem)
                -- traceShowM (start, size)
                -- readBytes slice 0 (IOVector.length slice) >>= traceShowM
                readBytes slice 0 size
              monitor (counterexample $ show actual <> " /= " <> show expected)
              assert $ actual == expected

      context "given an offset that is too far" $ do
        it "returns OutOfMemory error" $ property $
          \word (Capacity capacity) (Positive offset) ->
            monadicIO $ do
              let offset' = maxOffset + offset
              Left err <- run $ do
                mem <- newMemory capacity
                try $ Memory.writeWord offset' word mem
              assert $ err == OutOfMemory (offset', word)

    describe "readWord" $ do
      it "reads the word that has been written" $ property $
        \word (Capacity capacity) (Positive offset) ->
          monadicIO $ do
            word' <- run $ do
              mem <- newMemory capacity >>= Memory.writeWord offset word
              Memory.readWord offset mem
              -- Memory.toHex mem >>= traceShowM
            assert $ word' == word

    describe "readValue" $ do
      it "reads the value that has been written" $ property $
        \(Capacity capacity) (Positive v) (Positive offset) ->
          monadicIO $ do
            value <- run $ do
              mem <- newMemory capacity >>= Memory.write offset v
              Memory.read offset mem
              -- Memory.toHex mem >>= traceShowM
            assert $ value == v
