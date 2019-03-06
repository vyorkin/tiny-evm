{-# LANGUAGE TemplateHaskell #-}

module TinyEVM.VM.Instruction
  ( -- * The @Instruction@ type
    Instruction(..)
    -- * Lenses
  , operation
  , opcode
  , metadata
  , operands
    -- * Operations
  , mkInstr
  , mkInstrN
  , encode
  , decodeOne
    -- * Instructions
  , add
  , mul
  , sub
  , div
  , sdiv
  , mod
  , smod
  , addMod
  , mulMod
  , signExtend
  , push
  , pop
  , mload
  , mstore
  , sload
  , sstore
    -- * Re-exports
  , module TinyEVM.VM.Instruction.Metadata
  , module TinyEVM.VM.Instruction.Opcode
  , module TinyEVM.VM.Instruction.Operation
  ) where

import Prelude hiding (div, mod)

import Control.Lens (makeLenses)

import TinyEVM.VM.Instruction.Metadata (Metadata)
import TinyEVM.VM.Instruction.Opcode (InvalidOpcode(..), Opcode)

import TinyEVM.VM.Instruction.Operation (Operation (..))
import qualified TinyEVM.VM.Instruction.Operation as Operation

-- | Represents a VM instruction.
data Instruction = Instruction
  { _operation :: !Operation
  , _opcode    :: !Opcode
  , _metadata  :: !Metadata
  , _operands  :: ![Word8]
  } deriving (Eq, Show)

makeLenses ''Instruction

encode :: Instruction -> [Word8]
encode (Instruction _ b _ bs) = b:bs

-- | Decodes a single TinyEVM instruction,
-- including the operation, its metadata and arguments.
--
-- Returns a tuple with the decoded instruction and
-- the list of bytes representing the rest of the program.
--
-- Otherwise returns the `InvalidOpcode` error if the given list of bytes
decodeOne :: Opcode -> [Word8] -> Either InvalidOpcode (Instruction, [Word8])
decodeOne byte bytecode = do
  op <- Operation.decode byte
  let (args, rest) = decodeOperands op bytecode
  return (mkInstr op args, rest)

-- | Decodes instruction operands.
decodeOperands :: Operation -> [Word8] -> ([Word8], [Word8])
decodeOperands (Push n) bytecode = splitAt n bytecode
decodeOperands _ bytecode        = ([], bytecode)

mkInstr :: Operation -> [Word8] -> Instruction
mkInstr op args = Instruction
  { _operation = op
  , _opcode    = Operation.encode op
  , _metadata  = Operation.metadata op
  , _operands  = args
  }

mkInstrN :: Operation -> Instruction
mkInstrN = flip mkInstr []

push :: Int -> [Word8] -> Instruction
push n = mkInstr (Push n)

-- TODO: use TH, possibly

add, mul, sub, div, sdiv, mod, smod, addMod, mulMod,
  signExtend, pop, mload, mstore, sload, sstore :: Instruction

add = mkInstrN Add
mul = mkInstrN Mul
sub = mkInstrN Sub
div = mkInstrN Div
sdiv = mkInstrN SDiv
mod = mkInstrN Mod
smod = mkInstrN SMod
addMod = mkInstrN AddMod
mulMod = mkInstrN MulMod
signExtend = mkInstrN SignExtend
pop = mkInstrN Pop
mload = mkInstrN MLoad
mstore = mkInstrN MStore
sload = mkInstrN SLoad
sstore = mkInstrN SStore
