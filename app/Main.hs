module Main where

import Protolude
import BlockchainsQueryApi

import Network.Wai.Handler.Warp

main :: IO ()
main = loadBTConfig >>= startApp

startApp :: BTConfig -> IO ()
startApp conf = do
  putStrLn $ ("Listening on port " :: Text) <> show portNumber
  ctx <- mkContext conf
  run portNumber $ mkServer ctx
  where
    portNumber = fromIntegral $ port conf
