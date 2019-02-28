{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeOperators #-}

module VM.Instruction.OperationSpec (spec) where

import Generic.Random
import Test.Hspec
import Test.QuickCheck

import TinyEVM.VM.Instruction.Opcode (Opcode, isPush, push1, push32)
import TinyEVM.VM.Instruction.Operation (Operation (..), decode, encode)

-- For more info about genericArbitrary see:
-- https://hackage.haskell.org/package/generic-random-1.2.0.0/docs/Generic-Random-Tutorial.html

instance Arbitrary Operation where
  arbitrary = genericArbitraryUG customGens
    where
      customGens :: Gen Int :+ ()
      customGens = choose(1, 32) :+ ()

spec :: Spec
spec = describe "Operation" $ do
  it "roundtrips operations" $ property $ \op ->
    (decode . encode) op === Right op
