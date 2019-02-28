{-# LANGUAGE TemplateHaskell #-}

module Data.Example
  ( Example(..)
  , input
  , output
  ) where

import Control.Lens (makeLenses)

import TinyEVM.VM.Program (Program (..))

data Example = Example
  { _input  :: !Program
  , _output :: !Output
  } deriving (Show)

makeLenses ''Example

-- >>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- Just (fromList [("bar",2),("foo",1)])
