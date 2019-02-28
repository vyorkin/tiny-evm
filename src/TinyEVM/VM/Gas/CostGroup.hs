module TinyEVM.VM.Gas.CostGroup
  ( CostGroup
  , toInteger
  , zero
  , base
  , verylow
  , low
  , mid
  , costGroup
  ) where

import Prelude hiding (group, toInteger)

import TinyEVM.VM.Instruction.Operation (Operation (..))

-- | Represents a cost group according to the H.1 of the YP.
-- Each cost group applies to a subset of instructions.
data CostGroup
  = Zero    -- ^ W_zero
  | Base    -- ^ W_base
  | VeryLow -- ^ W_verylow
  | Low     -- ^ W_low
  | Mid     -- ^ W_mid
  deriving (Ord, Eq, Show)

-- | Get an amount of gas to pay for the given cost group.
toInteger :: CostGroup -> Integer
toInteger group = case group of
  Zero    -> 0
  Base    -> 2
  VeryLow -> 3
  Low     -> 5
  Mid     -> 8

-- | Operations of the set W_zero.
zero :: [Operation]
zero = [Stop]

-- | Operations of the set W_base.
base :: [Operation]
base = [Pop]

-- | Operations of the set W_verylow.
verylow :: [Operation]
verylow = [Add, Sub, MLoad, MStore]

-- | Operations of the set W_low.
low :: [Operation]
low = [Mul, Div, SDiv, Mod, SMod, SignExtend]

-- | Operations of the set W_mid.
mid :: [Operation]
mid = [AddMod, MulMod]

-- | Get a cost group for the given operation.
costGroup :: Operation -> Maybe CostGroup
costGroup (Push _) = Just VeryLow
costGroup op
  | op `elem` zero    = Just Zero
  | op `elem` base    = Just Base
  | op `elem` verylow = Just VeryLow
  | op `elem` low     = Just Low
  | op `elem` mid     = Just Mid
  | otherwise         = Nothing
