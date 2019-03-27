{-# LANGUAGE TemplateHaskell #-}

module Sandbox.TH4 where

import Language.Haskell.TH
import Sandbox.TH3 (genCurries)

$(genCurries 20)

-- The equation $([| e |]) = e holds for
-- all expressions e and similar equations hold for
-- declarations, and types

ex1 :: Int
ex1 = $([| id |]) 1
