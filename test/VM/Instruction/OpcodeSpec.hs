module VM.Instruction.OpcodeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import TinyEVM.VM.Instruction.Opcode (Opcode, isPush, push1, push32)
import qualified TinyEVM.VM.Instruction.Opcode as Opcode

spec :: Spec
spec = describe "Opcode" $ do
  it "returns the correct arity for PUSH opcodes" $
    forAll (choose (1, 32)) $ \n ->
      Opcode.arity (push1 + fromIntegral (n - 1)) === n
