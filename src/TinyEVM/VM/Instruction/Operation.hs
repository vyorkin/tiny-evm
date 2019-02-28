{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module TinyEVM.VM.Instruction.Operation
  ( -- * The @Operation@ type
    Operation(..)
    -- * Operations
  , encode
  , decode
  , metadata
  ) where

import TinyEVM.VM.Instruction.Metadata (Metadata, (|>))
import TinyEVM.VM.Instruction.Opcode (InvalidOpcode (..), Opcode)
import qualified TinyEVM.VM.Instruction.Opcode as Opcode

data Operation
  = Stop
  | Add
  | Mul
  | Sub
  | Div
  | SDiv
  | Mod
  | SMod
  | AddMod
  | MulMod
  | SignExtend
  | Push Int
  | Pop
  | MLoad
  | MStore
  | SLoad
  | SStore
  deriving (Eq, Ord, Generic, Show)

-- | Lookup metadata for the given operation.
metadata :: Operation -> Metadata
metadata = \case
  Stop       -> 0 |> 0
  Add        -> 2 |> 1
  Mul        -> 2 |> 1
  Sub        -> 2 |> 1
  Div        -> 2 |> 1
  SDiv       -> 2 |> 1
  Mod        -> 2 |> 1
  SMod       -> 2 |> 1
  AddMod     -> 2 |> 1
  MulMod     -> 2 |> 1
  SignExtend -> 2 |> 1
  Push _     -> 0 |> 1
  Pop        -> 1 |> 0
  MLoad      -> 1 |> 1
  MStore     -> 2 |> 0
  SLoad      -> 1 |> 1
  SStore     -> 2 |> 0

encode :: Operation -> Opcode
encode = \case
  Stop       -> 0x00
  Add        -> 0x01
  Mul        -> 0x02
  Sub        -> 0x03
  Div        -> 0x04
  SDiv       -> 0x05
  Mod        -> 0x06
  SMod       -> 0x07
  AddMod     -> 0x08
  MulMod     -> 0x09
  SignExtend -> 0x0b
  Push n     -> Opcode.push1 + fromIntegral (n - 1)
  Pop        -> 0x50
  MLoad      -> 0x51
  MStore     -> 0x52
  SLoad      -> 0x54
  SStore     -> 0x55

decode :: Opcode -> Either InvalidOpcode Operation
decode = \case
  0x00 -> Right Stop
  0x01 -> Right Add
  0x02 -> Right Mul
  0x03 -> Right Sub
  0x04 -> Right Div
  0x05 -> Right SDiv
  0x06 -> Right Mod
  0x07 -> Right SMod
  0x08 -> Right AddMod
  0x09 -> Right MulMod
  0x0b -> Right SignExtend
  0x50 -> Right Pop
  0x51 -> Right MLoad
  0x52 -> Right MStore
  0x54 -> Right SLoad
  0x55 -> Right SStore
  b | Opcode.isPush b -> Right $ Push $ Opcode.arity b
  b -> Left $ InvalidOpcode b
