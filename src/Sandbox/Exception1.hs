module Sandbox.Exception1 where

data MyException = ThisException | ThatException
    deriving Show

instance Exception MyException

-- throw ThisException `catch` \e -> putStrLn ("Caught " ++ show (e :: MyException))
