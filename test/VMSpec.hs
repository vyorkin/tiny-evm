{-# LANGUAGE ScopedTypeVariables #-}

module VMSpec (spec_vm) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (vector)

import Control.Lens ((^.))
import Prelude hiding (add, state)

import TinyEVM.Env (Env, mkEnv, vm)
import TinyEVM.VM (VM (..), runVM)
import qualified TinyEVM.VM as VM
import TinyEVM.VM.Code (Code (..))
import TinyEVM.VM.Instruction (Instruction)
import TinyEVM.VM.Instruction.DSL (add, push)
import TinyEVM.VM.Program (Program (..))
import TinyEVM.VM.Stack (Stack (..))
import qualified TinyEVM.VM.Stack as Stack
import TinyEVM.VM.State (gas, memory, pc, stack, storage)
import TinyEVM.VM.Storage (Storage)
import qualified TinyEVM.VM.Storage as Storage

spec_vm :: Spec
spec_vm = parallel $ do
  describe "TinyEVM.VM" $ do
    it "executes a single pushN instruction" $ property $
      \(Positive n) -> forAll (vector $ n + 1) $ \xs -> do
         state <- runProgram [push (n + 1) xs]
         state ^. stack `shouldBe` Stack.fromList (reverse xs)
    -- it "executes pop instruction" $ property $ $
    --   \(Positive n) -> forAll ()

runProgram :: [Instruction] -> IO VM.State
runProgram is = do
  env <- mkEnv mempty (mkProgram is)
  VM.run env >> readIORef (env ^. vm)

mkProgram :: [Instruction] -> Program
mkProgram is = Program (Code is) 10000 Storage.empty
