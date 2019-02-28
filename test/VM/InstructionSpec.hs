{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.InstructionSpec
  ( ValidOp(..)
  , ValidChunk(..)
  , InvalidOpcode(..)
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck

import TinyEVM.VM.Instruction (Opcode, Operation, decodeOne, mkInstruction)
import qualified TinyEVM.VM.Instruction.Opcode as Opcode
import qualified TinyEVM.VM.Instruction.Operation as Operation
import VM.Instruction.OperationSpec ()

data ValidOp = ValidOp Operation Opcode
  deriving (Eq, Show)

instance Arbitrary ValidOp where
  arbitrary = do
    op <- arbitrary
    let byte = Operation.encode op
    return $ ValidOp op byte

data ValidChunk = ValidChunk Opcode Operation [Word8]
  deriving (Eq, Show)

instance Arbitrary ValidChunk where
  arbitrary = do
    (ValidOp op byte) <- arbitrary
    args <- vector $ Opcode.arity byte
    return $ ValidChunk byte op args

newtype InvalidOpcode = InvalidOpcode Word8
  deriving (Eq, Show)

instance Arbitrary InvalidOpcode where
  arbitrary = InvalidOpcode <$> choose (0x80, 0xFF)

spec :: Spec
spec = describe "Instruction" $ do
  context "given a valid bytecode chunk" $ do
    it "decodes properly" $ property $ \(ValidChunk byte op rest) ->
      decodeOne byte rest === Right (mkInstruction byte op rest, [])
