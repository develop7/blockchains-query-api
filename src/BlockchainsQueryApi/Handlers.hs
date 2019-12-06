{-# LANGUAGE ScopedTypeVariables #-}

module BlockchainsQueryApi.Handlers 
    ( tx
    , Tx
    , balance
    , currentBlock
    , feeEstimate
    ) where

import BlockchainsQueryApi.Prelude
import BlockchainsQueryApi.Domain
import BlockchainsQueryApi.AppM

import Servant

feeEstimate :: Currency -> AppM FeeEstimate
feeEstimate currency = do
  node <- withNode currency
  render $ nodeFeeEstimate node

currentBlock :: Currency -> AppM Block
currentBlock currency = do
  node <- withNode currency
  render $ nodeCurrentBlock node

balance :: Currency -> Text -> AppM Balance
balance currency address = do
  node <- withNode currency
  render $ nodeBalance node address

tx :: Currency -> Text -> AppM Tx
tx currency paramHash = do 
  node <- withNode currency
  render $ nodeTransaction node paramHash

withNode :: Currency -> AppM (Node AppM)
withNode currency = 
  asks getNode >>= (&) currency

render :: AppM (Either Error a) -> AppM a
render errorOrResult = errorOrResult >>= either renderError pure

renderError :: Error -> AppM a
renderError e = 
  logError >> throwError err
  where 
    err = case e of 
            NotFound msg -> err404 { errBody = toS msg }
            InvalidInput msg -> err422 { errBody = toS msg }
            _ -> err500 { errBody = "Some other error: " <> show e }
    logError = pushLogEntry $ show err