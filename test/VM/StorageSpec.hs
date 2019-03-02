{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.StorageSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.UnorderedContainers ()

import TinyEVM.VM.Storage (Storage (..))
import qualified TinyEVM.VM.Storage as Storage

instance Arbitrary Storage where
  arbitrary = Storage <$> arbitrary

spec :: Spec
spec = describe "Storage" $ do
  it "gets what it puts" $ property $ \s k (Positive v) ->
    Storage.get (Storage.put s k v) k === Just v

  it "clears the value on put zero" $ property $ \s k (Positive v) ->
    Storage.get (Storage.put (Storage.put s k v) k 0) k === Nothing

