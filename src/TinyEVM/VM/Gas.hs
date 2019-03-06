module TinyEVM.VM.Gas
  ( cost
  ) where


import Prelude hiding (empty)

import Control.Lens ((^.))

import TinyEVM.VM.Gas.CostGroup (costGroup)
import qualified TinyEVM.VM.Gas.CostGroup as CostGroup
import TinyEVM.VM.Instruction (Instruction, Operation (..), operation)

-- | Represents an error that
-- might occur when estimating operation cost.
data GasError = InsufficientGasError Operation (Int, Int)
  deriving (Eq, Show)

instance ToText GasError where
  toText (InsufficientGasError op (actual, needed)) = unlines
    [ "Not enough gas to execute the "
    , show op
    , " operation. The actual gas is "
    , show actual
    , " but needed "
    , show needed
    ]

-- We don't estimate the real cost of `MSTORE` and `SSTORE` for simplicity.
-- In the current implementation `MSTORE` always costs 3 gas and `SSTORE` always costs 5000 gas.

-- | Returns the cost for the given `Instruction`.
-- This is defined in Appendix H of the Yellow Paper, Eq.(294) and is denoted `C`.
cost :: Instruction -> Maybe Integer
cost = calc . (^. operation)
 where
   calc :: Operation -> Maybe Integer
   calc SLoad  = Just 50
   calc SStore = Just 5000
   calc op     = CostGroup.toInteger <$> costGroup op
