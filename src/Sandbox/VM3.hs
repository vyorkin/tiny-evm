{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Sandbox.VM3 (main) where

import Control.Lens (makeLenses)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import qualified Data.List as List

data Instruction
  = Stop
  | Add Int Int
  | Mul Int Int
  | Sub Int Int
  | Div Int Int
  | Pop Int

type Stack = [Int]
type Program = [Instruction]

data State = State
  { _program :: !Program
  , _pc      :: !Int
  , _stack   :: !Stack
  }

makeLenses ''State

main :: IO ()
main = return ()
