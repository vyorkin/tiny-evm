{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Sandbox.Mixed2 where

import qualified Data.Map as Map

class IsFoo a

data X = X
data Y = Y

instance IsFoo X
instance IsFoo Y

data Foo = forall a. (IsFoo a) => Foo a

foos :: [Foo]
foos = [Foo X, Foo Y]

newtype FooMap k = FooMap (Map k Foo)

foos' :: FooMap String
foos' = FooMap
  $ Map.insert "y" (Foo Y)
  $ Map.singleton "x" (Foo X)
