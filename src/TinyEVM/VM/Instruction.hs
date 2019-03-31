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
  , isHalt
    -- * Re-exports
  , module TinyEVM.VM.Instruction.Metadata
  , module TinyEVM.VM.Instruction.Opcode
  , module TinyEVM.VM.Instruction.Operation
  ) where

import Prelude hiding (div, mod)

import Control.Lens (makeLenses)

import TinyEVM.VM.Instruction.Metadata (Metadata)
import TinyEVM.VM.Instruction.Opcode (InvalidOpcode (..), Opcode)
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

-- | Converts the given instruction to the
-- corresponding bytecode (opcode + operands).
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

-- | Checks wWhether the given `Instruction` is a normal halting.
isHalt :: Instruction -> Bool
isHalt (Instruction Stop _ _ _) = True
isHalt _ = False
