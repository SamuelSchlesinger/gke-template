{-# LANGUAGE OverloadedStrings #-}
module Server.Persistence.Redis where

import qualified Database.Redis as Redis

defaultRedisConnectInfo :: Redis.ConnectInfo
defaultRedisConnectInfo = Redis.defaultConnectInfo
  { Redis.connectHost = "redis.default.svc.cluster.local"
  , Redis.connectAuth = Just "password"
  , Redis.connectMaxIdleTime = 20
  , Redis.connectTimeout = Just 2000000000000
  }
