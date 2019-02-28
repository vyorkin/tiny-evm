{-# LANGUAGE TemplateHaskell #-}

module TinyEVM.VM.Instruction.Metadata
  ( Metadata
  , inputs
  , outputs
  , (|>)
  ) where

import Control.Lens (makeLenses)

-- | Represents an instruction metadata.
data Metadata = Metadata
  { -- | A number of inputs that a particular instruction requires.
    _inputs  :: !Int
    -- | A number of outputs that instruction will produce.
  , _outputs :: !Int
  } deriving (Eq, Show)

makeLenses ''Metadata

infixr 5 |>

-- | Creates a new `Metadata` by the given number of inputs and outputs.
(|>) :: Int -> Int -> Metadata
(|>) = Metadata
