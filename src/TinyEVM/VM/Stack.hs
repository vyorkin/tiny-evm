module TinyEVM.VM.Stack
  ( -- * The @Stack@ type
    Stack(..)
  , StackError(..)
    -- * Operations
  , empty
  , push
  , pushN
  , pop
  , popN
  , peek
  , maxSize
  ) where

import Prelude hiding (empty)

import Data.Foldable (foldrM)

-- | Represents a VM stack.
newtype Stack = Stack { unStack :: [Integer] }
  deriving (Eq, Show)

-- | Represents an error that
-- might occur when working with `Stack`.
data StackError
  -- | Used when the stack overflows
  -- because it contains too many items.
  = StackOverflow Integer
  -- | Used when the stack doesn't have enough items.
  | StackUnderflow
  deriving (Eq, Show)

instance ToText StackError where
  toText StackUnderflow = "Stack underflow"
  toText (StackOverflow v) = unlines
    [ "Stack overflow when attempting to push value: "
    , show v
    ]

-- | Creates a new empty stack.
empty :: Stack
empty = Stack []

-- | Pop most recently added item without removing from the `Stack`.`
peek :: Stack -> Either StackError (Integer, Stack)
peek (Stack (x:xs)) = Right (x, Stack (x:xs))
peek _              = Left StackUnderflow

-- | Pushes value to the stack.
push :: Integer -> Stack -> Either StackError Stack
push x (Stack xs) 
  | length xs >= maxSize = Left $ StackOverflow x
  | otherwise = Right $ Stack (x:xs)

-- | Pushes multiple values to the stack.
pushN :: [Integer] -> Stack -> Either StackError Stack
pushN xs stack = foldlM (flip push) stack xs

-- | Pops value from the stack,
-- returns that value and a new stack with value popped.
pop :: Stack -> Either StackError (Integer, Stack)
pop (Stack (x:xs)) = Right (x, Stack xs)
pop _              = Left StackUnderflow

-- | Pops multiple values off of stack,
-- returning these values and a new stack less that many elements.
-- Returns a `StackUnderflow` if the given stack contains insufficient elements.
popN :: Int -> Stack -> Either StackError ([Integer], Stack)
popN n (Stack xs)
  | length xs >= n = let (ys, xs') = splitAt n xs in Right (ys, Stack xs')
  | otherwise = Left StackUnderflow

-- | Stack has a maximum size of 1024 elements.
maxSize :: Int
maxSize = 1024
