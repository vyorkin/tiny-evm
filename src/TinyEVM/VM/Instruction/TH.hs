{-# LANGUAGE TemplateHaskell #-}

module TinyEVM.VM.Instruction.TH
  ( mkNullary
  ) where

import Prelude hiding (exp)
import qualified Data.Char as Char
import Language.Haskell.TH

import TinyEVM.VM.Instruction (Instruction(..), mkInstrN)

mkNullary :: Name -> Q [Dec]
mkNullary typeName = do
  TyConI (DataD _ _ _ _ ctors _) <- reify typeName
  concat . catMaybes <$> traverse handleCon ctors
  where
    handleCon :: Con -> Q (Maybe [Dec])
    handleCon (NormalC ctor []) = Just <$> mkDec ctor
    handleCon _ = return Nothing

mkDec :: Name -> Q [Dec]
mkDec ctor = do
  let body = [| mkInstrN $(conE ctor) |]
  sig <- sigD name [t| Instruction |]
  exp <- funD name [clause [] (normalB body) []]
  return [sig, exp]
  where
    name :: Name
    name = mkName $ Char.toLower <$> nameBase ctor
