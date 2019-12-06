module Main where

import Protolude

import Test.Hspec.Runner
import System.Process

import qualified BTCApiSpec as BTC
import qualified ETHApiSpec as ETH
import qualified ParitySpec as Parity

main :: IO ()
main = do
    withParity $
        hspecWith defaultConfig ETH.spec
    hspecWith defaultConfig specsWithoutMockServer
    where
        specsWithoutMockServer = BTC.spec >> Parity.spec

withParity :: IO () -> IO ()
withParity action = 
    withCreateProcess
        (proc "parity" ["-c", "dev-insecure"])
        (\_ _ _ _ -> threadDelay 10000000 >> action)