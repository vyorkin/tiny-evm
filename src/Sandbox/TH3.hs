{-# LANGUAGE TemplateHaskell #-}

module Sandbox.TH3 where

import Language.Haskell.TH

curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec

mkCurryDec :: Int -> Q Dec
mkCurryDec i = do
  fn <- curryN i
  let name = mkName $ "curry" ++ show i
  return $ FunD name [Clause [] (NormalB fn) []]

-- using syntax construction functions

genCurries' :: Int -> Q [Dec]
genCurries' n = forM [1..n] mkCurryDec'

mkCurryDec' :: Int -> Q Dec
mkCurryDec' i = do
  let name = mkName $ "curry" ++ show i
  funD name [clause [] (normalB (curryN i)) []]

-- quotation brackets

genId :: Q Exp
genId = do
  x <- newName "x"
  lamE [varP x] (varE x)

genId' :: Q Exp
genId' = [| \x -> x |]

-- expressions [e| .. |] or just [||]
-- patterns [p| .. |]
-- declarations [d| .. |]
-- types [t| .. |]
