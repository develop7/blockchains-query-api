{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module BlockchainsQueryApi.AppM 
  ( AppM
  , AppCtx (..)
  , BTConfig (..)
  , pushLogEntry
  , mkContext
  , loadBTConfig
  ) where

import BlockchainsQueryApi.Domain
import BlockchainsQueryApi.Haskoin
import BlockchainsQueryApi.Parity
import BlockchainsQueryApi.Ripple
import BlockchainsQueryApi.Prelude

import Data.String (String)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)

import Data.Aeson (encode)
import Data.Time.Clock          (UTCTime, getCurrentTime)
import Control.AutoUpdate       ( defaultUpdateSettings
                                , mkAutoUpdate
                                , updateAction
                                )

import System.Log.FastLogger                      ( ToLogStr(..)
                                                  , LoggerSet
                                                  , defaultBufSize
                                                  , newStdoutLoggerSet
                                                  , pushLogStrLn )

import Servant
import Env
import Network.URI (parseURI)

type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx { config :: BTConfig
                     , getNode :: Currency -> AppM (Node AppM)
                     , getLogger :: LoggerSet
                     , getTime :: IO UTCTime
                     }

data BTConfig = BTConfig { port :: Integer
                         , haskoinBtcUri :: Maybe URI
                         , haskoinBchUri :: Maybe URI
                         , parityUri :: Maybe URI
                         , rippleUri :: Maybe URI
                         } deriving (Show)


data LogMessage = LogMessage {
    ltime        :: !UTCTime     
  , lmessage     :: !Text
  , level        :: !Text
  , lversion     :: !Text
  , lenvironment :: !Text
} deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1 } ''LogMessage)

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

mkContext :: BTConfig -> IO AppCtx
mkContext conf = 
  AppCtx conf (nodeForCurrency conf)
    <$> mkLogger
    <*> mkGetTime

nodeForCurrency :: BTConfig -> Currency -> AppM (Node AppM)
nodeForCurrency conf currency = 
  maybe error pure mNode
    where
      error = throwError err500 { errBody = "Currency not configured: " <> show currency }
      mNode :: Maybe (Node AppM)
      mNode = 
        case currency of
          BTC -> haskoin <$> haskoinBtcUri conf
          BCH -> haskoin <$> haskoinBchUri conf
          ETH -> parity <$> parityUri conf
          XRP -> ripple <$> rippleUri conf

loadBTConfig :: IO BTConfig
loadBTConfig =
  Env.parse (header "Blockchains Query Api") $
    BTConfig <$> var (auto <=< nonempty) "BQ_PORT" (help "Public port for the http server")
           <*> var (parseUrl <=< nonempty) "BQ_BTC_NODE_URI" (def Nothing <> help "HTTP URI to reach the haskoin BTC interface")
           <*> var (parseUrl <=< nonempty) "BQ_BCH_NODE_URI" (def Nothing <> help "HTTP URI to reach the haskoin BCH interface")
           <*> var (parseUrl <=< nonempty) "BQ_ETH_NODE_URI" (def Nothing <> help "HTTP URI to reach the parity RPC interface")
           <*> var (parseUrl <=< nonempty) "BQ_XRP_NODE_URI" (def Nothing <> help "HTTP URI to reach the rippled RPC interface")

parseUrl :: String -> Either Env.Error (Maybe URI)
parseUrl url = 
    case parseURI $ toS url of
      Nothing -> panic $ "Invalid URL: " <> toS url
      Just u -> Right $ Just u

mkLogger :: IO LoggerSet
mkLogger = newStdoutLoggerSet defaultBufSize

mkGetTime :: IO (IO UTCTime) 
mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}

pushLogEntry :: Text -> AppM ()
pushLogEntry msg = do
  logset <- asks getLogger
  getTimeFromAutoUpdate <- asks getTime
  time <- liftIO getTimeFromAutoUpdate

  let logMsg = LogMessage { ltime = time
      , lmessage = msg
      , level = "info"
      , lversion = "1.1.1"
      , lenvironment = "development"
      }
  liftIO $ pushLogStrLn logset $ toLogStr logMsg
