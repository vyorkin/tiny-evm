{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}

module TinyEVM.VM
  ( VM (..)
  , MonadVM
  , runVM
  , run
  , main
  , module TinyEVM.VM.State
  ) where

import Prelude hiding (mod, state)

import Colog (Message, WithLog)
import Control.Exception (throw)
import Control.Lens (over, set, view, (%~), (.~), (^.), (^?))
import Control.Lens.At (at, ix)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Throw.Extra (eitherThrow)
import Control.Natural (type (~>))
import qualified Data.Text as Text
import Text.Show (show)

import TinyEVM.Env (Env (..), code, trace, vm)
import TinyEVM.VM.Code (Code (..))
import TinyEVM.VM.Exception (InsufficientGasException (..),
                             InvalidOpcodeException (..), StackException (..),
                             UnsupportedOperationException (..))
import qualified TinyEVM.VM.Gas as Gas
import TinyEVM.VM.Instruction (Instruction (..), InvalidOpcode (..),
                               Metadata (..), Opcode (..), Operation (..),
                               isHalt, metadata, opcode, operands, operation)
import TinyEVM.VM.Instruction.DSL (stop)
import TinyEVM.VM.Instruction.Metadata (inputs, outputs)
import TinyEVM.VM.Memory (Memory, MemoryException (..))
import qualified TinyEVM.VM.Memory as Memory
import TinyEVM.VM.Stack (Stack, StackError (..))
import qualified TinyEVM.VM.Stack as Stack
import TinyEVM.VM.State (State, gas, memory, pc, stack, storage)
import qualified TinyEVM.VM.Storage as Storage

newtype VM a = VM
  { unVM :: ReaderT (Env VM) IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Env VM)
    , MonadThrow
    )

runVM :: Env VM -> VM ~> IO
runVM env = usingReaderT env . unVM

run :: Env VM -> IO ()
run = flip runVM main

-- | Synonym for constraints commonly
-- satisfied by monads used in stack.
type MonadVM m =
  ( WithLog (Env VM) Message m
  , MonadIO m
  , MonadThrow m
  )

-- | Runs a single execution cycle returning the new state,
-- defined as `O` in the Yellow Paper, Eq.(143).
main :: MonadVM m => m ()
main = do
  i <- fetch
  exec (i ^. operation) (i ^. operands) >> spend i
  when (not $ isHalt i) main

-- | Fetches the current instruction
-- and updates the program counter (index of the current instruction).
fetch :: MonadVM m => m Instruction
fetch = do
  env <- ask
  let ref = env ^. vm
  state <- readIORef ref
  modifyIORef ref $ pc %~ (+ 1)
  let
    is = unCode $ env ^. code
    i = is ^? ix (state ^. pc)
  return $ fromMaybe stop i

-- | Esimates the instruction cost and
-- reduces the amount of gas left accordingly.
spend :: MonadVM m => Instruction -> m ()
spend i = do
  ref <- view vm
  left <- view gas <$> readIORef ref
  let
    cost = fromMaybe 0 (Gas.cost i)
    remaining = left - cost
  when (remaining < 0) $ throwM $
    InsufficientGasException (i ^. operation) (left, cost)
  modifyIORef ref $ gas .~ remaining

-- | Executes the operation given its arguments.
--
-- Runs a single execution cycle returning the new state,
-- defined as `O` in the Yellow Paper, Eq.(143).
exec :: MonadVM m => Operation -> [Word8] -> m ()
exec Stop _ = return () -- Normal halting
exec (Push _) xs = withStack (Stack.pushN xs) (, ())
exec Pop [_] = return ()
exec MLoad [offset] = undefined
--   ref <- view vm
--   state <- readIORef ref
--   value <- Memory.read (state ^. memory) offset
--   newStack <- Stack.push value (state ^. stack)
exec MStore [offset, value] = undefined
exec SLoad [key] = undefined
exec SStore [key, value] = undefined
exec op args =
  let r = eval op args
  in withStack (Stack.push r) (, ())

-- | Given a simple arithmetic operation and
-- its operands performs the caculation and returns the result.
eval :: Operation -> [Word8] -> Word8
eval Add (x:y:_)      = x + y
eval Mul (x:y:_)      = x * y
eval Sub (x:y:_)      = x - y
eval Div (_:0:_)      = 0
eval Div (x:y:_)      = x `div` y
eval AddMod (_:_:0:_) = 0
eval AddMod (x:y:z:_) = (x + y) `rem` z
eval MulMod (_:_:0:_) = 0
eval MulMod (x:y:z:_) = (x * y) `rem` z
eval Mod (_:0:_)      = 0
eval Mod (x:y:_)      = x `rem` y
eval op args          = throw $ UnsupportedOperationException (op, args)

-- | Helper function for executing `Stack`-related operations.
-- There are 3 types of results when working with `Stack`:
-- * `(Word8, Stack)`
-- * `([Word8], Stack)`
-- * `Stack
-- But we want to have a general function, for convenience.
-- Thats why there is a `(a -> (Stack, b))`,
-- it is a kind of normalizer function which unifies the result.
withStack
  :: MonadVM m
  => (Stack -> Either StackError a) -- ^ Stack modifier function
  -> (a -> (Stack, b))              -- ^ Result normalizer function
  -> m b
withStack fn cb = do
  ref <- view vm
  oldStack <- view stack <$> readIORef ref
  (newStack, val) <- cb <$> eitherThrow StackException (fn oldStack)
  modifyIORef ref $ stack .~ newStack
  return val
