module TinyEVM.VM.Instruction.Opcode
  ( -- * The @Opcode@ type
    Opcode
  , InvalidOpcode(..)
  , push1
  , push32
  , isPush
  , arity
  ) where

import Prelude hiding (Show, show)

import Text.Show (Show, show)

-- | Operation code of this toy VM.
-- Specifies the operation to be performed.
type Opcode = Word8

-- | An error that might occur when
-- we encounter unknown/invalid opcode.
newtype InvalidOpcode = InvalidOpcode Opcode
  deriving (Eq)

instance Show InvalidOpcode where
  show (InvalidOpcode op) = "Invalid opcode: " ++ show op

push1 :: Opcode
push1 = 0x60

push32 :: Opcode
push32 = push1 + 31

isPush :: Opcode -> Bool
isPush x = x >= push1 && x <= push32

-- | Returns the opcode arity.
arity :: Opcode -> Int
arity op
  | isPush op = fromIntegral (op - push1) + 1
  | otherwise = 0
