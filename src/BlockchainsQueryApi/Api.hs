{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module BlockchainsQueryApi.Api
    ( server
    , api
    , mkServer

    -- ** re-exports
    , serve
    , Proxy
    , API
    ) where

import BlockchainsQueryApi.Handlers
import BlockchainsQueryApi.AppM
import BlockchainsQueryApi.Domain
import BlockchainsQueryApi.Prelude

import Control.Lens ((.~), (?~))
import Data.Swagger
import Servant
import Servant.Swagger

type API = 
    Capture "currency" Currency :> "tx" :> Capture "hash" Text :> Get '[JSON] Tx
    :<|> Capture "currency" Currency :> "balance" :> Capture "address" Text :> Get '[JSON] Balance
    :<|> Capture "currency" Currency :> "current-block" :> Get '[JSON] Block
    :<|> Capture "currency" Currency :> "fee-estimate" :> Get '[JSON] FeeEstimate

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type APIWithSwagger = API :<|> SwaggerAPI

api :: Proxy API
api = Proxy

apiWithSwagger :: Proxy APIWithSwagger
apiWithSwagger = Proxy

swaggerDoc :: AppM Swagger
swaggerDoc = pure $ toSwagger api
  & info.title   .~ "Blockchains API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API to query transaction information in several blockchain nodes"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

server :: ServerT APIWithSwagger AppM
server = (tx :<|> balance :<|> currentBlock :<|> feeEstimate) :<|> swaggerDoc

nt :: AppCtx -> AppM a -> Handler a
nt s x = runReaderT x s

mkServer :: AppCtx -> Application
mkServer s = serve apiWithSwagger $ hoistServer apiWithSwagger (nt s) server