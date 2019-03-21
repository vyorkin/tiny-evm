{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.StackSpec (spec_stack) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (vector)

import TinyEVM.VM.Stack (Stack (..), StackError (..), unStack)
import qualified TinyEVM.VM.Stack as Stack

instance Arbitrary Stack where
  arbitrary = Stack <$> arbitrary

spec_stack :: Spec
spec_stack = parallel $ do
  describe "TinyEVM.VM.Stack" $ do
    it "pushes a value" $ property $ \x stack ->
      Stack.push x stack === Right (Stack $ x : unStack stack)

    it "pushes multiple values" $ property $ \(NonEmpty xs) stack ->
      Stack.pushN xs stack === Right (Stack $ reverse xs ++ unStack stack)

    it "pops a value" $ property $ \x xs ->
      Stack.pop (Stack (x:xs)) === Right (x, Stack xs)

    it "pops several values" $ property $ \(Positive n) (Positive k) ->
      forAll (vector $ n + k) $ \xs ->
        Stack.popN k (Stack xs) === Right (take k xs, Stack $ drop k xs)

    it "checks for overflows" $ property $ \(Positive k) x ->
      forAll (vector $ Stack.maxSize + k) $ \xs ->
        Stack.push x (Stack xs) === Left (StackOverflow x)

    it "checks for underflows" $ property $ \(Positive k) ->
      forAll (vector k) $ \xs ->
        Stack.popN (k + 1) (Stack xs) === Left StackUnderflow
