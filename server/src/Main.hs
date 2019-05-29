{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, encode, decode)

import Control.Monad (forM_, forever)

import Data.Text (Text)
import qualified Data.Text as T

import Control.Concurrent (MVar, newMVar, modifyMVar_,
                           modifyMVar, readMVar)
import Control.Exception (finally)

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

data Coordinate = Coordinate {
    row :: Int
  , col :: Int
  } deriving (Generic, Show)
instance ToJSON Coordinate
instance FromJSON Coordinate

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  msg <- WS.receiveData conn
  let move = decode msg :: Maybe Coordinate
  clients <- readMVar state
  case move of
    Nothing ->
      print "Recieved incorrect message"
    Just c ->
      flip finally disconnect $ do
        modifyMVar_ state $ \s -> do
          let s' = addClient client s
          broadcast (fst client `mappend` " joined") s'
          return s'
        talk client state
      where
        client = ("Player", conn)
        disconnect = do
          s <- modifyMVar state $ \s ->
            let s' = removeClient client s in return (s', s')
          broadcast (fst client `mappend` " disconnected") s

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast msg

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  print message
  forM_ clients $ \(_, conn) ->
    WS.sendTextData conn message
