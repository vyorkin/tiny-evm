module Data.List.Extra
  ( padLeft
  , padRight
  ) where

padLeft :: Int -> a -> [a] -> [a]
padLeft n y xs = replicate (n - length xs) y ++ xs

padRight :: Int -> a -> [a] -> [a]
padRight n y xs = xs ++ replicate (n - length xs) y
