{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Word256Spec (spec_word256) where

import Test.Hspec
import Test.QuickCheck

import Data.Word256 (Word256(..), fromInt, toInt)
import qualified Data.Word256 as Word256

instance Arbitrary Word256 where
  arbitrary = Word256.fromInt . (+ 1) . getPositive <$> arbitrary

spec_word256 :: Spec
spec_word256 = parallel $
  describe "Data.Word256" $ do
    describe "size" $ do
      it "returns the size/length of the word in bytes" $ do
        Word256.size (fromInt 42) `shouldBe` 2
        Word256.size (fromInt 242) `shouldBe` 3
        Word256.size (fromInt 4242) `shouldBe` 4

    describe "fromInt" $ do
      it "makes a new Word256" $ do
        fromInt 242 `shouldBe` (Word256 $ replicate 29 0 ++ [50, 52, 50])

    describe "toInt" $ do
      it "converts to Int" $ do
        toInt (Word256 $ replicate 29 0 ++ [50, 52, 50]) `shouldBe` 242
