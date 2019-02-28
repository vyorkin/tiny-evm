{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.CodeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import TinyEVM.VM.Code (Code (..), decode, encode)
import VM.InstructionSpec (ValidChunk (..))

data ValidBytecode = ValidBytecode [Word8]
  deriving (Eq, Show)

instance Arbitrary ValidBytecode where
  arbitrary = ValidBytecode . toBytecode <$> arbitrary
    where
      toBytecode (ValidChunk opcode _ args) = opcode:args

spec :: Spec
spec = describe "Code" $ do
  context "given a valid bytecode" $ do
    it "roundtrips code" $ property $ \(ValidBytecode bs) ->
      (encode <$> decode bs) === Right bs
