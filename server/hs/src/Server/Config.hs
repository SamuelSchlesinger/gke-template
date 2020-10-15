{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Server.Config
( Context
, connectionPool
, Config
, connectionString
, defaultConfig
, setConnectionString
, createContext
, AppM
, postgresTx
, postgres
, redis
) where

import Data.String (IsString)
import qualified Squeal.PostgreSQL
import Data.Pool (Pool)
import qualified Data.Pool
import Data.ByteString (ByteString)
import Server.Persistence.Postgres (CurrentSchemata, defaultConnectionString)
import Server.Persistence.Redis (defaultRedisConnectInfo)
import Data.Coerce (coerce)
import Control.Monad.Reader (ReaderT)
import Database.Redis (ConnectInfo)
import qualified Database.Redis
import Server.Prelude
import Server.Cryptography.RSA

type AppM = ReaderT Context IO

data RedisTxError = RedisTxError [Char] | RedisTxTimeout
  deriving Show

instance Exception RedisTxError

postgres :: Squeal.PostgreSQL.PQ CurrentSchemata CurrentSchemata IO a -> AppM a
postgres x = do
  cp <- connectionPool <$> ask
  liftIO $ Squeal.PostgreSQL.usingConnectionPool cp x

postgresTx :: Squeal.PostgreSQL.PQ CurrentSchemata CurrentSchemata IO a -> AppM a
postgresTx x = do
  postgres (Squeal.PostgreSQL.transactionallyRetry txMode x)
  where
    txMode = Squeal.PostgreSQL.defaultMode
      { Squeal.PostgreSQL.isolationLevel = Squeal.PostgreSQL.Serializable
      }

redis :: Database.Redis.Redis a -> AppM a
redis x = do
  cp <- redisConnectionPool <$> ask
  liftIO $ Data.Pool.withResource cp (flip Database.Redis.runRedis x)

redisTx :: Database.Redis.RedisTx (Database.Redis.Queued a) -> AppM a
redisTx x = go 10 where
  go n
    | n <= 0 = throwM RedisTxTimeout
    | otherwise = do
      cp <- redisConnectionPool <$> ask
      redis (Database.Redis.multiExec x) >>= \case
        Database.Redis.TxSuccess a -> pure a
        Database.Redis.TxAborted -> go (n - 1)
        Database.Redis.TxError errStr -> throwM (RedisTxError errStr)

data Context = Context
  { connectionPool :: Pool (Squeal.PostgreSQL.K Squeal.PostgreSQL.Connection CurrentSchemata)
  , redisConnectionPool :: Pool Database.Redis.Connection
  , rsaContext :: RSAContext
  }

data Config = Config
  { connectionString :: ConnectionString
  , redisConnectInfo :: ConnectInfo
  , rsaConfig :: RSAConfig
  }

defaultConfig :: Config
defaultConfig = Config
  { connectionString =
      ConnectionString defaultConnectionString
  , redisConnectInfo =
      defaultRedisConnectInfo
  , rsaConfig = RSAConfig { publicExponent = 65537, rsaKeySize = 4096 }
  }

newtype ConnectionString = ConnectionString ByteString
  deriving newtype IsString

setConnectionString :: ConnectionString -> Config -> Config
setConnectionString connectionString config = config { connectionString }

createContext :: Config -> IO Context
createContext Config{connectionString, redisConnectInfo} = Context
  <$> Squeal.PostgreSQL.createConnectionPool (coerce connectionString) 10 10 10
  <*> Data.Pool.createPool (Database.Redis.checkedConnect redisConnectInfo) Database.Redis.disconnect 10 10 10
