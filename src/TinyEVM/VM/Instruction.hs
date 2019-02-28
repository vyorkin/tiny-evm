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
  , mkInstruction
  , encode
  , decodeOne
    -- * Re-exports
  , module TinyEVM.VM.Instruction.Metadata
  , module TinyEVM.VM.Instruction.Opcode
  , module TinyEVM.VM.Instruction.Operation
  ) where

import Control.Lens (makeLenses, (^.))

import TinyEVM.VM.Instruction.Metadata (Metadata)
import TinyEVM.VM.Instruction.Opcode (InvalidOpcode, Opcode)

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
encode i = (i ^. opcode):(i ^. operands)

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
  let
    (args, rest) = decodeOperands op bytecode
    instr = mkInstruction byte op args
  return (instr, rest)

-- | Decodes instruction operands.
decodeOperands :: Operation -> [Word8] -> ([Word8], [Word8])
decodeOperands (Push n) bytecode = splitAt n bytecode
decodeOperands _ bytecode        = ([], bytecode)

mkInstruction :: Opcode -> Operation -> [Word8] -> Instruction
mkInstruction byte op args =
  Instruction { _operation = op
              , _opcode    = byte
              , _metadata  = Operation.metadata op
              , _operands  = args
              }
