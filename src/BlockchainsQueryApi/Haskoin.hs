module BlockchainsQueryApi.Haskoin
    ( haskoin
    ) where

import BlockchainsQueryApi.Prelude
import BlockchainsQueryApi.Domain
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens ((^?), (^..))
import Network.URI
import Network.HTTP.Simple
import Conduit (MonadThrow)
import Data.String (String)

haskoin :: (MonadIO m,  MonadThrow m) => URI -> Node m
haskoin uri = Node 
                { nodeTransaction = getTransaction uri
                , nodeBalance = getBalance uri
                , nodeCurrentBlock = getCurrentBlock uri
                , nodeFeeEstimate = getFeeEstimate uri
                }

getFeeEstimate :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error FeeEstimate)
getFeeEstimate uri = undefined

getCurrentBlock :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error Block)
getCurrentBlock uri = do
    (status, jsonBody) <- mkRequest uriToString'
    txsAttempts <- mapM (getTransaction uri) (txIds jsonBody)
    pure $ case (status, sequence txsAttempts) of
        (200, Right txs) -> maybe (Left $ RPCError "Invalid block body") Right (responseToBlock jsonBody txs)
        _ -> Left $ RPCError $ "Some other error, haskoin response status: " <> show status
    where
        txIds :: Value -> [Text]
        txIds body = 
            body ^.. key "tx" . values . _String
        responseToBlock :: Value -> [Tx] -> Maybe Block
        responseToBlock body txs' = do
            rId <- body ^? key "hash" . _String
            rNumber <- body ^? key "height" . _Integer
            pure $ Block rId rNumber txs'

        uriToString' = "GET " <> uriToString identity uri "" <> "/block/best"

getBalance :: (MonadIO m,  MonadThrow m) => URI -> Address -> m (Either Error Balance)
getBalance uri address = do
    (status, jsonBody) <- mkRequest uriToString'
    pure $ case status of
        200 -> maybe (Left $ RPCError "Invalid balance body") Right (responseToBalance jsonBody)
        404 -> Left $ NotFound ("Could not find the address " <> toS address <> " in Haskoin")
        _ -> Left $ RPCError $ "Some other error, haskoin response status: " <> show status
    where
        uriToString' = "GET " <> uriToString identity uri "" <> "/address/" <> toS address <> "/balance"

responseToBalance :: Value -> Maybe Balance
responseToBalance body = do
    rAddress <- body  ^? key "address" . _String
    rConfirmed <- body  ^? key "confirmed" . _Integer
    pure $ Balance rAddress rConfirmed

getTransaction :: (MonadIO m,  MonadThrow m) => URI -> Text -> m (Either Error Tx)
getTransaction uri paramHash = do
    (status, jsonBody) <- mkRequest uriToString'
    pure $ case status of
        200 -> maybe (Left $ RPCError "Invalid transaction body") Right (responseToTx jsonBody)
        404 -> Left $ NotFound ("Could not find the hash " <> toS paramHash <> " in Haskoin")
        _ -> Left $ RPCError $ "Some other error, haskoin response status: " <> show status
    where
        uriToString' = "GET " <> uriToString identity uri "" <> "/transaction/" <> toS paramHash

mkRequest :: (MonadIO m,  MonadThrow m) => String -> m (Int, Value)
mkRequest uri = 
    parseRequest uri 
    >>= httpJSON 
    >>= (\r -> pure (getResponseStatusCode r, getResponseBody r))

responseToTx :: Value -> Maybe Tx
responseToTx body = do
    rTxId <- body  ^? key "txid" . _String
    rFee <- body  ^? key "fee" . _Integer
    let
        rFromAddresses = body ^.. key "inputs" . values . key "address" . _String
        rFromValues = body ^.. key "inputs" . values . key "value" . _Integer
        rToAddresses = body ^.. key "outputs" . values . key "address" . _String
        rToValues = body ^.. key "outputs" . values . key "value" . _Integer
    pure Tx
        { txHash = rTxId
        , txFee = rFee
        , txFrom = uncurry Balance <$> zip rFromAddresses rFromValues
        , txTo = uncurry Balance <$> zip rToAddresses rToValues
        }
