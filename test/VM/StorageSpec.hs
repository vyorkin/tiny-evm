{-# OPTIONS_GHC -fno-warn-orphans #-}

module VM.StorageSpec (spec_storage) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.UnorderedContainers ()

import TinyEVM.VM.Storage (Storage (..))
import qualified TinyEVM.VM.Storage as Storage

instance Arbitrary Storage where
  arbitrary = Storage <$> arbitrary

spec_storage :: Spec
spec_storage = parallel $ do
  describe "TinyEVM.VM.Storage" $ do
    it "gets what it puts" $ property $ \s k (Positive v) ->
      Storage.get (Storage.put s k v) k === Just v

    it "clears the value on put zero" $ property $ \s k (Positive v) ->
      Storage.get (Storage.put (Storage.put s k v) k 0) k === Nothing
