{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.InstructionSpec
  ( BadOpcode(..)
  , spec
  ) where

import Control.Lens ((^.))

import Test.Hspec
import Test.QuickCheck

import TinyEVM.VM.Instruction
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

spec :: Spec
spec = describe "Instruction" $ do
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

  context "given a valid bytecode" $ do
    it "decodes properly" $ property $ \i ->
      decodeOne (i ^. opcode) (i ^. operands) === Right (i, [])
