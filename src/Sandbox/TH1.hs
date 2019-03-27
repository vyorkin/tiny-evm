{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}

module Sandbox.TH1 where

import Language.Haskell.TH
import Data.List (intercalate)

import Prelude hiding (show)
import Text.Show (show)

listFields :: Name -> Q [Dec]
listFields typeName = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify typeName

  let
    names = map (\(name,_,_) -> name) fields

    showField :: Name -> Q Exp
    showField name = [|\x -> s ++ " = " ++ show ($(varE name) x)|] where
      s = nameBase name

    showFields :: Q Exp
    showFields = listE $ map showField names

  [d|instance Show $(conT typeName) where
     show x = intercalate ", " (map ($ x) $showFields)|]

-- [
--   SigD f_2 (VarT t_1)
-- , ValD (VarP f_2) (NormalB (LitE (StringL "123"))) []
-- ]

mkFunc :: String -> Q [Dec]
mkFunc str = do
  sig <- sigD str' [t| String |]
  val <- valD (varP str') (normalB (litE (stringL str))) []
  return [sig, val]
  where str' = mkName str
