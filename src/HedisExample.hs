{-# LANGUAGE OverloadedStrings #-}
module HedisExample where

import Control.Monad.Writer
import Database.Redis

f = do
  conn <- checkedConnect defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello,world)

