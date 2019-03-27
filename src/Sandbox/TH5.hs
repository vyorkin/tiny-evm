{-# LANGUAGE TemplateHaskell #-}

module Sandbox.TH5 where

import qualified Relude.Unsafe as Unsafe
import Data.Foldable (foldl)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (putQ)

-- reification example

data Result e a = Err e | Ok a
data List a = Nil | Cons (List a)
data Tree a = Leaf a | Node (Tree a) a (Tree a)

data Deriving = Deriving
  { tyCon :: Name
  , tyVar :: Name
  }

-- DataD
--   :: Cxt
--   -> Name
--   -> [TyVarBndr]
--   -> Maybe Kind
--   -> [Con]
--   -> [DerivClause]
--   -> Dec

-- NewtypeD
--   :: Cxt
--   -> Name
--   -> [TyVarBndr]
--   -> Maybe Kind
--   -> Con
--   -> [DerivClause]
--   -> Dec

deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty = do
  (TyConI tyCon) <- reify ty
  (tyConName, tyVars, cs) <- case tyCon of
    DataD    _ nm tyVars _ cs _ -> return (nm, tyVars, cs)
    NewtypeD _ nm tyVars _ c  _ -> return (nm, tyVars, [c])
    _ -> fail "deriveFunctor: tyCon may not be a type synonym."
  let
    (KindedTV tyVar StarT) = Unsafe.last tyVars
    instanceType = conT ''Functor `appT`
      (foldl apply (conT tyConName) (Unsafe.init tyVars))
  putQ $ Deriving tyConName tyVar
  sequence [instanceD (return []) instanceType [genFmap cs]]
  where
    apply t (PlainTV name)    = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)

genFmap :: [Con] -> Q Dec
genFmap = undefined
-- genFmap cs = funD 'fmap (genFmapClause cs)

genFmapClause :: Con -> Q Clause
genFmapClause = undefined
