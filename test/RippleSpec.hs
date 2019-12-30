module RippleSpec
  ( spec
  ) where

import Test.Hspec

import Data.Aeson

import Data.Aeson (Value)

import Protolude

import BlockchainsQueryApi

import BlockchainsQueryApi.Ripple

import Network.URI


spec :: Spec
spec =
    describe  "getBalance" $
        it "should return with the address balance" $ do
          Right balance <- getBalance address endpoint
          balance `shouldBe` expectedBalance
        where
          address = "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh"
          Just endpoint = parseURI "http://localhost:5005"
          expectedBalance = Balance "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh" 100000000000000000
