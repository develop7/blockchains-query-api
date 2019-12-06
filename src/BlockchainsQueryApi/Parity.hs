{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module BlockchainsQueryApi.Parity
    ( parity
    -- exported for text only
    , responseToTx
    ) where

import BlockchainsQueryApi.Prelude
import BlockchainsQueryApi.Domain
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens ((^?))
import Network.URI
import Network.HTTP.Simple
import Conduit (MonadThrow)
import Data.Aeson.QQ
import Data.Either.Combinators (mapLeft)

data ParityRequest =
    ParityRequest
        { trmethod :: Text
        , trparams :: Value
        , trid :: Integer
        , trjsonrpc :: Text
        } deriving (Eq, Show)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''ParityRequest)

parity :: (MonadIO m,  MonadThrow m) => URI -> Node m
parity uri = Node 
                { nodeTransaction = getTransaction uri
                , nodeBalance = getBalance uri
                , nodeCurrentBlock = getCurrentBlock uri
                , nodeFeeEstimate = getFeeEstimate uri
                }

getFeeEstimate :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error FeeEstimate)
getFeeEstimate uri = undefined

getCurrentBlock :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error Block)
getCurrentBlock uri = do
    responseOrError <- mkRequest uri "eth_getBlockByNumber" [aesonQQ| ["latest", true] |]
    pure $ either (Left . RPCError . show) identity $ blockOrFailure <$> responseOrError
    where
        blockOrFailure :: Value -> Either Error Block
        blockOrFailure body =
            maybe (Left $ RPCError $ toS $ "Error decoding: " <> encode body) Right (responseToBlock body)

        responseToBlock :: Value -> Maybe Block
        responseToBlock body = do
            rId <- body  ^? key "result" . key "hash" . _String
            rNumber <- body  ^? key "result" . key "number" . _String
            rNumberNumeric <- fromIntegral <$> (readMaybe $ toS rNumber :: Maybe Integer)
            rTransactionValues <- body  ^? key "result" . key "transactions" . _Array
            let transactions = mapMaybe responseToTx $ toList rTransactionValues
            pure $ Block rId rNumberNumeric transactions

getBalance :: (MonadIO m,  MonadThrow m) => URI -> Address -> m (Either Error Balance)
getBalance uri address = do
    responseOrError <- mkRequest uri "eth_getBalance" [aesonQQ| [#{address}] |]
    pure $ either (Left . RPCError . show) identity $ balanceOrFailure <$> responseOrError
    where
        balanceOrFailure :: Value -> Either Error Balance
        balanceOrFailure body =
            maybe (Left $ RPCError $ toS $ "Error decoding: " <> encode body) Right (responseToBalance body)

        responseToBalance :: Value -> Maybe Balance
        responseToBalance body = do
            rBalance <- body ^? key "result" . _String
            rBalanceNumeric <- fromIntegral <$> (readMaybe $ toS rBalance :: Maybe Integer)
            pure $ Balance address rBalanceNumeric

getTransaction :: (MonadIO m,  MonadThrow m) => URI -> Text -> m (Either Error Tx)
getTransaction uri paramHash = do
    responseOrError <- mkRequest uri "eth_getTransactionByHash" [aesonQQ| [#{paramHash}] |]
    pure $ 
        either 
            (const $ Left $ NotFound ("Could not find the hash " <> paramHash <> " in parity")) 
            identity 
            $ txOrFailure <$> responseOrError
    where
        txOrFailure :: Value -> Either Error Tx
        txOrFailure body =
            case body  ^? key "result" . key "hash" . _String of
                Nothing -> Left $ NotFound ""
                _ -> maybe (Left $ RPCError $ toS $ "Error decoding: " <> encode body) Right (responseToTx body)
        
mkRequest :: (MonadIO m,  MonadThrow m) => URI -> Text -> Value -> m (Either Error Value)
mkRequest uri method params = do 
    jsonRequest <- setRequestBodyJSON parityRequest <$> parseRequest uri'
    responseOrError <- httpJSONEither jsonRequest
    pure $ getResponseBody $ mapError <$> responseOrError
    where
        mapError = mapLeft (const $ RPCError "Error decoding response")
        uri' = "POST " <> uriToString identity uri ""
        parityRequest :: ParityRequest
        parityRequest = ParityRequest method params 2 "2.0"

responseToTx :: Value -> Maybe Tx
responseToTx body = do
    rTxId <- body  ^? key "result" . key "hash" . _String
    rFrom <- body  ^? key "result" . key "from" . _String
    rTo <- body  ^? key "result" . key "to" . _String

    rGasPrice <- body ^? key "result" . key "gasPrice" . _String
    rGasPriceNumeric <- fromIntegral <$> (readMaybe $ toS rGasPrice :: Maybe Integer)
    rGas <- body ^? key "result" . key "gas" . _String
    rGasNumeric <- fromIntegral <$> (readMaybe $ toS rGas :: Maybe Integer)
    rValue <- body ^? key "result" . key "value" . _String
    rValueNumeric <- fromIntegral <$> (readMaybe $ toS rValue :: Maybe Integer)

    let fee = rGasPriceNumeric * rGasNumeric
    pure Tx
        { txHash = rTxId
        , txFee = fee
        , txFrom = [Balance rFrom (rValueNumeric + fee)]
        , txTo = [Balance rTo rValueNumeric]
        }
