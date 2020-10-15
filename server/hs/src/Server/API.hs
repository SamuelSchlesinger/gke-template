{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server.API where

import Servant.API
import Server.Prelude

type GetUsers = Get '[JSON] [Text]
type PostUser = ReqBody '[JSON] Text :> PostNoContent
type Health = "health" :> GetNoContent

type API = GetUsers
  :<|> PostUser
  :<|> Health

theAPI :: Proxy API
theAPI = Proxy
