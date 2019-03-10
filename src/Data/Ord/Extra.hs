module Data.Ord.Extra (clamp) where

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx
