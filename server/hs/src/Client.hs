{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Client (fuzzer) where

import Data.Time (getCurrentTime)
import Server.Prelude
import Server.API
import Data.Proxy
import Servant.API
import qualified Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Data.Text

fuzzer :: IO ()
fuzzer = do
  manager <- newManager defaultManagerSettings
  baseUrl <- Servant.Client.parseBaseUrl "server.default.svc.cluster.local"
  let clientEnv = Servant.Client.mkClientEnv manager baseUrl
  let run a =  Servant.Client.runClientM a clientEnv >>= \case
        Left err -> putStrLn (show err)
        Right x -> putStrLn (show x)
  let getUsers :<|> postUser :<|> health = Servant.Client.client (Proxy @API)
  forM_ [1..5] \i -> forkIO $ forever do
    threadDelay (10000)
    t <- getCurrentTime
    run getUsers
    run (postUser (Data.Text.pack (show t <> show i)))
    run health
  threadDelay (60 * 1000000)
