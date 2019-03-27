{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}

module Sandbox.TH2 where

import Sandbox.TH1
import Language.Haskell.TH (mkName)

data MyData = MyData
  { foo :: String
  , bar :: Int
  }

-- emptyShow (mkName "MyData")
-- emptyShow ''MyData

listFields ''MyData

ex1 :: IO ()
ex1 = print $ MyData { foo = "bar", bar = 5 }

$(mkFunc "haskell")
