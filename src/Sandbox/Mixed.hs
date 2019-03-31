module Sandbox.Mixed where

foo :: Integral a => [a] -> String
foo source =
  let item = length . filter (==1) $ source
  in if (item > 0) then "1" else "0"
