{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module BlockchainsQueryApi.Domain
    ( Tx (..)
    , Currency (..)
    , Block (..)
    , Balance (..)
    , Address
    , Node (..)
    , FeeEstimate (..)
    ) where

import BlockchainsQueryApi.Prelude
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import Conduit (MonadThrow)
import Data.Swagger (ToSchema, ToParamSchema)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

type Address = Text

data Currency = BTC | BCH | ETH deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''Currency)
instance ToSchema Currency
instance ToParamSchema Currency
instance FromHttpApiData Currency
  where
    parseUrlPiece "eth" = Right ETH
    parseUrlPiece "btc" = Right BTC
    parseUrlPiece "bch" = Right BCH
    parseUrlPiece _ = Left "Coin not supported"
instance ToHttpApiData Currency
  where
    toUrlPiece ETH = "eth"
    toUrlPiece BTC = "btc"
    toUrlPiece BCH = "bch"

data Balance =
  Balance 
    { blAddress :: Text
    , blValue :: Integer
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 2 } ''Balance)
instance ToSchema Balance

data Tx = 
  Tx 
    { txHash :: Text
    , txFee :: Integer
    , txFrom :: [Balance]
    , txTo :: [Balance]
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 2 } ''Tx)
instance ToSchema Tx

data Block = 
  Block 
    { blHash :: Text
    , blIndex :: Integer
    , blTransactions :: [Tx]
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 2 } ''Block)
instance ToSchema Block

data FeeEstimate = 
  FeeEstimate
    { feCurrency :: Currency
    , feEstimate :: Float
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 2 } ''FeeEstimate)
instance ToSchema FeeEstimate

data Node m = 
  Node 
    { nodeTransaction :: (MonadIO m,  MonadThrow m) => Text -> m (Either Error Tx)
    , nodeBalance :: (MonadIO m,  MonadThrow m) => Address -> m (Either Error Balance)
    , nodeCurrentBlock :: (MonadIO m,  MonadThrow m) => m (Either Error Block)
    , nodeFeeEstimate :: (MonadIO m,  MonadThrow m) => m (Either Error FeeEstimate)
    }