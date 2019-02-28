{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.CodeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import TinyEVM.VM.Code (Code (..), decode, encode)
import TinyEVM.VM.Instruction (Instruction, add, opcode, operands, push)
import qualified TinyEVM.VM.Instruction as Instruction
import VM.InstructionSpec ()

instance Arbitrary Code where
  arbitrary = Code <$> arbitrary

spec :: Spec
spec = describe "Code" $ do
  context "given a valid bytecode" $ do
    it "roundtrips code" $ property $ \code ->
      (decode . encode) code === Right code

    it "decodes correctly" $ do
      let code = Code [push 1 [5], push 1 [4], add]
      decode [96, 5, 96, 4, 1] === Right code
