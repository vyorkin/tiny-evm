{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.InstructionSpec
  ( BadOpcode(..)
  , spec_instruction
  ) where

import Test.Hspec
import Test.QuickCheck

import Control.Lens ((^.))

import TinyEVM.VM.Instruction
import TinyEVM.VM.Instruction.DSL
import TinyEVM.VM.Instruction.Metadata ((|>))
import qualified TinyEVM.VM.Instruction.Opcode as Opcode
import qualified TinyEVM.VM.Instruction.Operation as Operation
import VM.Instruction.OperationSpec ()

newtype BadOpcode = BadOpcode Word8
  deriving (Eq, Show)

instance Arbitrary BadOpcode where
  arbitrary = BadOpcode <$> choose (0x80, 0xFF)

instance Arbitrary Instruction where
  arbitrary = do
    op <- arbitrary
    args <- genOperands op
    return $ mkInstr op args

genOperands :: Operation -> Gen [Word8]
genOperands = vector . Operation.arity

spec_instruction :: Spec
spec_instruction = parallel $ do
  describe "TinyEVM.VM.Instruction" $ do
    it "makes simple instructions" $ do
      add `shouldBe` Instruction
        { _operation = Add
        , _opcode    = 0x01
        , _metadata  = 2 |> 1
        , _operands  = []
        }
      push 2 [3, 7] `shouldBe` Instruction
        { _operation = Push 2
        , _opcode    = 0x61
        , _metadata  = 0 |> 1
        , _operands  = [3, 7]
        }

    describe "decodeOne" $ do
      context "given a valid opcode and operands" $ do
        it "decodes properly" $ property $ \i ->
          decodeOne (i ^. opcode) (i ^. operands) === Right (i, [])

      context "when provided with invalid opcode" $ do
        it "returns an InvalidOpcode error" $ property $ \(BadOpcode op) args ->
          decodeOne op args === Left (InvalidOpcode op)
