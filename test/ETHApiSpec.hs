{-# LANGUAGE QuasiQuotes #-}
module ETHApiSpec (spec) where

import Protolude hiding (get)

import BlockchainsQueryApi hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.URI

spec :: Spec
spec =
    with (mkServer <$> mkContext conf) $ do
            describe "GET /:currency/balance/:address" $ do
                it "responds with 200 when given address is found" $
                    get "/eth/balance/0x00a329c0648769a73afac7f9381e08fb43dbea72" `shouldRespondWith` 200
                it "responds with 0 Balance when given address is not found" $
                    get "/eth/balance/0x00a329c0648769a73afac7f9381e08fb43dbea71" `shouldRespondWith` [json|{"address":"0x00a329c0648769a73afac7f9381e08fb43dbea71","value":0}|]

            describe "GET /:currency/tx/:hash" $ do
                it "responds with 404 when given hash is not a valid transaction hash" $
                    get "/eth/tx/invalid-hash" `shouldRespondWith` 404
                it "responds with 404 when given hash is not found" $
                    get "/eth/tx/0xad54c43d670eb9c5758fdfc9ce508899123af791042206fd619ccc0ac07c37dd" `shouldRespondWith` 404

            describe "GET /:currency/current-block" $
                it "responds with 200 and block information" $
                    get "/eth/current-block" `shouldRespondWith` 200

    where
        conf = BTConfig 
                { port = 8080
                , haskoinBtcUri = parseURI "https://btc.haskoin.com/api"
                , haskoinBchUri = parseURI "https://btc.haskoin.com/api"
                , parityUri = parseURI "http://localhost:8545"
                }
