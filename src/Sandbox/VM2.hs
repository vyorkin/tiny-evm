{-# LANGUAGE ScopedTypeVariables #-}

module Sandbox.VM2 (main, example, program) where

import Control.Monad.Morph (generalize, hoist)
import qualified Data.List as List

type Eval a = State [Int] a

runEval :: [Int] -> Eval a -> a
runEval = flip evalState

pop :: Eval Int
pop = do
  top <- gets List.head
  modify List.tail
  return top

push :: Int -> Eval ()
push x = modify (x:)

program :: Eval Int
program = do
  push 3
  push 4
  pop
  pop

example :: StateT [Int] IO ()
example = do
  (result :: _) <- hoist generalize program
  liftIO $ putStrLn $ "result: " ++ show result

main :: IO ()
main = return ()
