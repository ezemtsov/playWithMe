{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game

import Control.Concurrent

import Web.Scotty
import Network.Wai.Middleware.Static
import qualified Network.WebSockets as WS

main :: IO ()
main = do
  state <- newMVar newServerState
  forkIO (WS.runServer "127.0.0.1" 9160 $ gameServer state)
  scotty 8080 $ webApp

webApp = do
  middleware $ staticPolicy (noDots >-> addBase "web")
  get "/" $ file "./web/index.html"
