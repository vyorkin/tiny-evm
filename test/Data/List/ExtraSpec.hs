module Data.List.ExtraSpec (spec_list) where

import Test.Hspec
import Test.QuickCheck

import Data.List.Extra (padLeft, padRight)

spec_list :: Spec
spec_list = parallel $
  describe "Data.List.Extra" $ do
    describe "padLeft" $ do
      it "pads list from the left" $ property $
        \(NonEmpty xs) (Positive n) e ->
          let
            padding = n + 1
            size = length xs + padding
            list = padLeft size (e :: Int) xs
          in
            take padding list === replicate padding e

    describe "padRight" $ do
      it "pads list from the right" $ property $
        \(NonEmpty xs) (Positive n) e ->
          let
            padding = n + 1
            size = length xs + padding
            list = padRight size (e :: Int) xs
          in
            drop (length xs) list === replicate padding e
