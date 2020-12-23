{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}

module Sandbox.Mixed where

import Control.Concurrent (ThreadId, forkIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.Map as Map

foo :: Integral a => [a] -> String
foo source =
  let item = length . filter (==1) $ source
  in if (item > 0) then "1" else "0"

-- data Service
--   = Telegram { offset :: !Int }
--   | VKontakte { ts :: !Int, server :: !Text }
--   deriving (Show)

-- data Model (a :: Service) = Model
--   { serviceData  :: !Service
--   , serviceToken :: !Text
--   }

-- run :: Model -> IO ()
-- run model@Model { serviceData = Telegram {} } = undefined
-- run _ = undefined

-- run cfg@Config {service = VKontakte {}} (Right logger) =
--   runBot (prepareModel logger cfg :: Model VKontakte)
-- run _ (Left err) = runMainLoop $ Left err

newtype StateLambda a = Lambda (Integer -> (a, Integer))

instance Functor StateLambda where
  fmap f (Lambda g) = Lambda $ \x ->
    let (a, y) = g x in (f a, y)

data Env = Env
  { num :: Int
  }

bar :: ReaderT Env Maybe Int
bar = do
  n <- asks num
  x <- lift $ baz
  return $ n + x

baz :: Maybe Int
baz = do
  x <- Just 5
  y <- Just 37
  return $ x + y

#define MY_OWN_CLASS (Eq, Ord, Show, Read)

data X = A | B | C
     deriving MY_OWN_CLASS

nestedIO :: IO (IO ())
nestedIO = do
  putStrLn "Hello!"
  let !a = trace "test" ()
  return (putStrLn "Hello again!")

class (MonadReader AppConfig m) => HasConfig m where
    getConfig :: m AppConfig

class (MonadIO m) => HasIO m where
    fork :: IO () -> m ThreadId

instance HasIO App where
    fork = liftIO . forkIO

instance HasIO IO where
    fork = forkIO

instance HasConfig App where
    getConfig = ask

data AppConfig = AppConfig { appTelegramReq     :: Int
                           , appTelegramUpdRate :: Int }

newtype App a = App { unApp :: ReaderT AppConfig IO a }
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadIO
                         , MonadUnliftIO
                         , MonadReader AppConfig)

runApp :: AppConfig -> App a -> IO a
runApp env = usingReaderT env . unApp

type IM m = (HasIO m, HasConfig m, MonadUnliftIO m)

telegramThread :: IM m => m ()
telegramThread = undefined

slackThread :: IM m => m ()
slackThread = undefined

run :: IM m => m ()
run = do
  config <- getConfig
  void $ withRunInIO $ \io -> do
    telegramThreadId <- fork $ io telegramThread
    slackThreadId    <- fork $ io slackThread
    return ()
