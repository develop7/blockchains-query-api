{-# LANGUAGE QuasiQuotes #-}
module BTCApiSpec (spec) where

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
                it "responds with 200 when given hash is found" $
                    get "/btc/balance/1Cdid9KFAaatwczBwBttQcwXYCpvK8h7FK" `shouldRespondWith` 200
                it "responds with proper general Balance body when given hash is found" $
                    get "/btc/balance/1Cdid9KFAaatwczBwBttQcwXYCpvK8h7FK" `shouldRespondWith` [json|{"address":"1Cdid9KFAaatwczBwBttQcwXYCpvK8h7FK","value":10330143}|]
                it "responds with 500 if node has no uri configured" $
                    get "/bch/balance/1Cdid9KFAaatwczBwBttQcwXYCpvK8h7FK" `shouldRespondWith` 500

            describe "GET /:currency/tx/:hash" $ do
                it "responds with 404 when given hash is not a valid transaction hash" $
                    get "/btc/tx/invalid-hash" `shouldRespondWith` 404
                it "responds with 404 when given hash is not found" $
                    get "/btc/tx/ad54c43d670eb9c5758fdfc9ce508899123af791042206fd619ccc0ac07c37dd" `shouldRespondWith` 404
                it "responds with 200 when given hash is found" $
                    get "/btc/tx/0ce33f35bd4c6ec6ddb96ec54d6ae2e943a4bf1fdba4189650d35ed81d11b9d7" `shouldRespondWith` 200
                it "responds with proper general Tx body when given hash is found" $
                    get "/btc/tx/0ce33f35bd4c6ec6ddb96ec54d6ae2e943a4bf1fdba4189650d35ed81d11b9d7" `shouldRespondWith` [json|{"hash":"0ce33f35bd4c6ec6ddb96ec54d6ae2e943a4bf1fdba4189650d35ed81d11b9d7","fee":24400,"from":[{"address":"1A7tWftaGHohhGcJMVkkm4zAYnF53KjRnU", "value":83472149},{"address":"1A7tWftaGHohhGcJMVkkm4zAYnF53KjRnU","value":2000000000}],"to":[{"address":"3DNAeaFJDM7Rt77T3MdugHg7f8wH3g8ern","value":1234827256},{"address":"37epBQNkx9785XVVnRWPLWs8iDhPSauiAw","value":105153447},{"address":"1A7tWftaGHohhGcJMVkkm4zAYnF53KjRnU","value":743467046}]}|]
                it "responds with 500 if node has no uri configured" $
                    get "/bch/tx/0ce33f35bd4c6ec6ddb96ec54d6ae2e943a4bf1fdba4189650d35ed81d11b9d7" `shouldRespondWith` 500
            describe "GET /:currency/fee-estimate" $
                it "responds with 200 and block information" $
                    get "/btc/fee-estimate" `shouldRespondWith` 200
            xdescribe "GET /:currency/current-block (we should set a test node before enabling this)" $
                it "responds with 200 and block information" $
                    get "/btc/current-block" `shouldRespondWith` 200
    
    where
        conf = BTConfig 
                { port = 8080
                , haskoinBtcUri = parseURI "https://btc.haskoin.com/api"
                , haskoinBchUri = Nothing
                , parityUri = parseURI "http://localhost:8546"
                }
