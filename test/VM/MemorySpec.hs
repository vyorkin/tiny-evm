module VM.MemorySpec (spec_memory) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Monadic (assert, monadicIO, run, monitor)

import qualified Data.Vector.Unboxed.Mutable as IOVector

import Data.Word256 (Word256)
import qualified Data.Word256 as Word256
import TinyEVM.VM.Memory (Memory (..), MemoryError (..), newMemory, vec, pos, maxOffset)
import qualified TinyEVM.VM.Memory as Memory

spec_memory :: Spec
spec_memory = parallel $ do
  describe "TinyEVM.VM.Memory" $ do
    describe "newMemory" $ do
      it "respects the given capacity" $ property $ \(Positive c) ->
        monadicIO $ do
          let capacity = c + 1
          mem  <- run $ newMemory capacity
          let
            len = IOVector.length (vec mem)
            expectedLen = capacity * Word256.bytes
          monitor (counterexample $ show len <> " /= " <> show expectedLen)
          assert $ len == expectedLen

    describe "writeWord" $ do
      context "given a valid offset" $ do
        it "writes correctly" $ property $ \(Positive x) ->
          monadicIO $ do
            let
              w = Word256.fromInt (x + 1)
              size = Word256.size w
            mem <- run $ newMemory 10
            (Right mem') <- run $ Memory.writeWord mem 0 w
            let v = IOVector.slice (Word256.bytes - size) size (vec mem')
            actual <- run $ traverse (IOVector.read v) [0..size]
            let expected = Word256.significant w
            monitor (counterexample $ show actual <> " /= " <> show expected)
            assert $ actual == expected

      context "given an offset that is too far" $ do
        it "returns OutOfMemory error" $ pending
          -- property $ \mem (Positive delta) bytes ->
          --   let offset = maxOffset + delta
 --   in Memory.writeBytes mem offset bytes === Left (OutOfMemory (offset, bytes))

    describe "readWord" $ do
      it "reads word that has been written" $ pending
        -- property $ \mem (Positive offset) bytes ->
        -- (flip Memory.readBytes offset <$> Memory.writeBytes mem offset bytes) === Right bytes

    describe "readValue" $ do
      it "reads value that has been written" $ pending
        -- property $ \mem (Positive offset) val ->
        -- (flip Memory.read offset <$> Memory.write mem offset val) === Right val
