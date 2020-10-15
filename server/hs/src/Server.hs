{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Server where

import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO)
import Control.Monad.Except (ExceptT(ExceptT))

import Control.Exception (catches, Handler(Handler), SomeException)

import Servant (Get, JSON, serve, NoContent(..), ServerT, hoistServer, err500, ServerError, (:<|>)(..), ReqBody, (:>), Post, GetNoContent)
import qualified Servant
import Network.Wai.Handler.Warp (run)

import Data.Proxy (Proxy(Proxy))
import Data.ByteString (ByteString)
import Data.Text (Text)

import Squeal.PostgreSQL

import Database.Redis (ping)
import qualified Database.Redis as Redis

import System.IO (hPutStrLn, stderr, stdout)

import Server.Persistence.Postgres
import Server.Persistence.Redis
import Server.Config
import Server.Prelude
import Server.API

server :: ServerT API AppM
server = getUsers :<|> createUser :<|> health where
  createUser user = do
    x <- redis ping
    postgresTx $
      executeParams_ (Manipulation aParam genericRow $ insertInto_ #user (Values_ (Set (param @1) `as` #id))) user
    pure NoContent
  getUsers = do
    xs <- postgresTx $
      execute (Query nilParams #id $ select_ (#u ! #id `as` #id) (from (table (#user `as` #u))))
        >>= getRows
    pure xs
  health = do
    x <- redis ping
    guard (x == Right Redis.Pong) 
    pure NoContent

transform :: Context -> (forall x. AppM x -> Servant.Handler x)
transform context appM = (Servant.Handler . ExceptT)
  ((Right <$> runReaderT appM context) `catches`
    [ Handler \(e :: ServerError) -> do
        pure (Left e)
    , Handler \(e :: SomeException) -> do
        pure (Left err500)
    ]
  )

main :: IO ()
main = do
  context <- createContext defaultConfig
  let application = serve theAPI (hoistServer theAPI (transform context) server)
  run 9000 application
