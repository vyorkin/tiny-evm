module TinyEVM.VM.Gas
  ( cost
  ) where

import Prelude hiding (empty)

import Control.Lens (view)

import TinyEVM.VM.Gas.CostGroup (costGroup)
import qualified TinyEVM.VM.Gas.CostGroup as CostGroup
import TinyEVM.VM.Instruction (Instruction, Operation (..), operation)

-- We don't estimate the real cost of `MSTORE` and `SSTORE` for simplicity.
-- In the current implementation `MSTORE` always costs 3 gas and `SSTORE` always costs 5000 gas.

-- | Returns the cost for the given `Instruction`.
-- This is defined in Appendix H of the Yellow Paper, Eq.(294) and is denoted `C`.
cost :: Instruction -> Maybe Int
cost = calc . view operation
 where
   calc :: Operation -> Maybe Int
   calc SLoad  = Just 50
   calc SStore = Just 5000
   calc op     = CostGroup.toInt <$> costGroup op
