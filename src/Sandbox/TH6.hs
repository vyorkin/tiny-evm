{-# LANGUAGE TemplateHaskell       #-}

module Sandbox.TH6 where

import Language.Haskell.TH

myFunc :: Q Exp
myFunc = do
  x <- newName "x"
  return $ LamE
    [VarP x]
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))

-- λ> let f = (* 2) . $myFunc
-- f :: Num c => c -> c

-- λ> f 10 :: Int
-- 22

myFunc' :: Q Exp
myFunc' = [| \x -> x + 1 |]

add2 :: Q Exp
add2 = [| $myFunc' . $myFunc |]
