{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Web.Scotty
import Network.Wai.Middleware.Static
import qualified Network.WebSockets as WS

import Router

-- Our main function just starts the web server
main :: IO ()
main = do
  state <- newMVar Router.newRouterState
  forkIO (WS.runServer "0.0.0.0" 8080 $ startRouter state)
  scotty 80 $ webApp

webApp = do
  middleware $ staticPolicy (noDots >-> addBase "web")
  -- Root route should bring to Landging page
  get "/:session" $ file "./web/index.html"
