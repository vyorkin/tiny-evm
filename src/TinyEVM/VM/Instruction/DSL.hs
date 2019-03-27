{-# LANGUAGE TemplateHaskell #-}

module TinyEVM.VM.Instruction.DSL
  ( stop
  , add
  , mul
  , sub
  , div
  , sdiv
  , mod
  , smod
  , addmod
  , mulmod
  , signextend
  , push
  , pop
  , mload
  , mstore
  , sload
  , sstore
  ) where

import Prelude hiding (div, mod)

import TinyEVM.VM.Instruction.TH (mkNullary)
import TinyEVM.VM.Instruction (Instruction(..), mkInstr)
import TinyEVM.VM.Instruction.Operation (Operation(..))

push :: Int -> [Word8] -> Instruction
push n = mkInstr (Push n)

mkNullary ''Operation
