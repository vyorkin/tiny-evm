{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Sandbox.VM1 (main) where

import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import qualified Data.List as List

type Stack = [Int]
type Output = [Int]
type Program = [Instruction]

data Instruction
  = Push Int
  | Pop
  | Puts

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype App a = App { runApp :: VM a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Program
    , MonadWriter Output
    , MonadState Stack
    )

evalInstruction :: Instruction -> App ()
evalInstruction Pop      = modify List.tail
evalInstruction (Push x) = modify (x:)
evalInstruction Puts     = do
  x <- gets List.head
  tell [x]

eval :: App ()
eval = do
  instr <- ask
  case instr of
    []     -> return ()
    (i:is) -> evalInstruction i >> local (const is) eval

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (runApp eval)

main :: IO ()
main = mapM_ print $ execVM example

example :: Program
example =
  [ Push 2
  , Push 10
  , Puts
  , Pop
  , Puts
  , Pop
  ]
