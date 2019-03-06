{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.CodeSpec (spec_code) where

import Test.Hspec
import Test.QuickCheck

import TinyEVM.VM.Code (Code (..), decode, encode)
import TinyEVM.VM.Instruction (Instruction, InvalidOpcode(..), add, opcode, operands, push)
import qualified TinyEVM.VM.Instruction as Instruction
import VM.InstructionSpec (BadOpcode(..))

instance Arbitrary Code where
  arbitrary = Code <$> arbitrary

spec_code :: Spec
spec_code = parallel $ do
  describe "TinyEVM.VM.Code" $ do
    context "when given a valid bytecode" $ do
      it "roundtrips code" $ property $ \code ->
        (decode . encode) code === Right code

      it "decodes correctly" $ do
        let code = Code [push 1 [5], push 1 [4], add]
        decode [96, 5, 96, 4, 1] `shouldBe` Right code

    context "when provided with invalid bytecode" $ do
      describe "decode" $ do
        it "returns an InvalidOpcode error" $ property $
          \(BadOpcode b1) (BadOpcode b2) ->
            decode [96, 5, b1, 96, 4, 1, b2] `shouldBe` Left (InvalidOpcode b1)
