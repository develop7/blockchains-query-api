module BlockchainsQueryApi.Client
    ( getTx
    , getBalance
    , getCurrentBlock
    , getFeeEstimate
    , Tx (..)
    , Currency (..)
    , Balance (..)
    , ClientError (..)
    ) where

import BlockchainsQueryApi.Prelude
import BlockchainsQueryApi.Api
import BlockchainsQueryApi.Domain

import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

tx :: Currency -> Text -> ClientM Tx
balance :: Currency -> Text -> ClientM Balance
currentBlock :: Currency -> ClientM Block
feeEstimate :: Currency -> ClientM FeeEstimate
tx :<|> balance :<|> currentBlock :<|> feeEstimate = client api

getTx :: Text -> Currency -> Text -> IO (Either ClientError Tx)
getTx endpoint currency txId = getInIO endpoint $ tx currency txId

getBalance :: Text -> Currency -> Text -> IO (Either ClientError Balance)
getBalance endpoint currency address = getInIO endpoint $ balance currency address

getCurrentBlock :: Text -> Currency -> IO (Either ClientError Block)
getCurrentBlock endpoint currency = getInIO endpoint $ currentBlock currency

getFeeEstimate :: Text -> Currency -> IO (Either ClientError FeeEstimate)
getFeeEstimate endpoint currency = getInIO endpoint $ feeEstimate currency

getInIO :: Text -> ClientM a -> IO (Either ClientError a)
getInIO endpoint cl = do
    url <- parseBaseUrl (toS endpoint)
    manager' <- newManager defaultManagerSettings
    runClientM cl (mkClientEnv manager' url)
