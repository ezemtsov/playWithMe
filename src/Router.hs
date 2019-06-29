 {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Router where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Control.Monad (forever)
import Control.Concurrent
import Control.Exception (finally)
import qualified Network.WebSockets as WS
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import GHC.Generics
import System.Random

import qualified TypesGameInput as I
import qualified TypesGameOutput as O
import qualified TypesGame as TG
import Game

----------------------------------------------------------------------
-- Here's an entry point to the app.

-- We need a router that allows to start independent game sessions
-- each time when host requests a new game. Each game session
-- must be stored in a router state so that guest players are able
-- to join specific session.

-- To make that possible we need to define router state
type RouterState = HashMap SessionId (MVar GameState)
type SessionId = [Char]

newRouterState :: RouterState
newRouterState = HM.empty

addSession :: SessionId -> MVar GameState
           -> RouterState -> RouterState
addSession = HM.insert

getSession :: SessionId -> RouterState
           -> Maybe (MVar GameState)
getSession = HM.lookup

data RouterMessage = Route (Maybe SessionId) I.Message
  deriving (Generic, Eq, Show)
instance FromJSON RouterMessage
instance ToJSON RouterMessage

data StateVersion = New | Existing
  deriving (Eq, Show)

fetchGameSession :: Maybe SessionId
                 -> WS.Connection
                 -> MVar RouterState
                 -> IO ( MVar GameState )
fetchGameSession maybeSession conn rState =
  case maybeSession of
    Just sId -> do
      -- Let's check if it exists in router state
      s <- readMVar rState
      case getSession sId s of
        Nothing -> do
          -- Session is not found
          newGame <- newMVar Game.newGameState
          -- Use that sessionId
          -- Update the router state
          modifyMVar_ rState $ \s ->
            return (addSession sId newGame s)
          -- Return new session
          return newGame

        Just gState ->
          -- Session is found
          return gState
    Nothing -> do
      -- User sent a message with no SessionId
      -- We'll need to create a new session
      newGame <- newMVar Game.newGameState
      -- Generate new sessionId
      sId <- genHash
      -- Send sessionId to the client
      let ctrlMsg = O.Message O.Game (O.NewSession sId)
      WS.sendTextData conn (encode ctrlMsg)
      -- Update the router state
      modifyMVar_ rState $ \s ->
        return (addSession sId newGame s)
      -- Return new session
      return newGame

-- Main game backend function
startRouter :: MVar RouterState -> WS.ServerApp
startRouter rState pending = do
  -- Accept connection
  conn <- WS.acceptRequest pending
  -- Check if connection is alive every 30 secs
  WS.forkPingThread conn 30

  -- Let's recieve the initial message
  msg <- WS.receiveData conn :: IO ByteString
  -- print ("init msg: " <> msg)
  let maybeMsg = decode msg :: Maybe RouterMessage
  case maybeMsg of
    Nothing -> incorrectMessage msg
    Just (Route maybeSession initMessage) -> do

      gState <- fetchGameSession maybeSession conn rState
      case initMessage of
        I.Connect (I.Player name) -> do
          let client = (name, conn)

          flip finally (disconnect client gState) $ do
            modifyMVar_ gState $ \s -> do
              let s' = Game.addClient client s
              -- Warn everybody
              let ctrlMsg = O.Message O.User (O.Connected name)
              Game.broadcast (encode ctrlMsg) s'
              return s'

            -- Start message exchange
            talk client gState
        _ -> print "Initial message expected"

-- Message parser and game logic
talk :: Client -> MVar GameState -> IO ()
talk (user, conn) state = forever $ do
  -- Recieve a message
  msg <- WS.receiveData conn :: IO ByteString
  -- print ("game msg" <> msg)
  -- Try to parse it as contol command
  let maybeMsg = decode msg :: Maybe RouterMessage
  case maybeMsg of
    Nothing -> incorrectMessage msg
    Just (Route maybeSession gameMessage) -> do
      Game.gameLogic (user, conn) gameMessage state

-- Disconnect from server
disconnect :: Client -> MVar GameState -> IO ()
disconnect client gState = do
  -- Remove client from state
  s <- modifyMVar gState $ \s ->
    let s' = removeClient client s in return (s',s')
    
  -- Broadcast that one of the players is disconnected
  let ctrlMsg = O.Message O.User
                (O.Disconnected $ fst client)
  broadcast (encode ctrlMsg) s
    
incorrectMessage :: ByteString -> IO ()
incorrectMessage msg =
  print ("Incorrect message: " <> msg)

genHash :: IO SessionId
genHash = do
  let hashChr = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
  xs <- sequenceA .
        take 10 .
        repeat $
        randomRIO (0,length hashChr - 1)
  return ((hashChr !!) <$> xs)
