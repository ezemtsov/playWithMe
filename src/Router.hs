{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Router where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict.InsOrd (toList)
import qualified Data.HashMap.Strict as HM hiding (toList)

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
import qualified TypesGame as G
import Game

----------------------------------------------------------------------
-- Here's an entry point to the app.

-- Router starts independent game sessions each time when host
-- requests a new game. Each game session must be stored in
-- a router state so that guest players are able to join specific session.

-- To make that possible we need to define router state
type RouterState = HashMap G.SessionId (MVar GameState)

newRouterState :: RouterState
newRouterState = HM.empty

addSession :: G.SessionId -> MVar GameState
           -> RouterState -> RouterState
addSession = HM.insert

getSession :: G.SessionId -> RouterState
           -> Maybe (MVar GameState)
getSession = HM.lookup

fetchGameState :: Maybe G.SessionId
                 -> MVar RouterState
                 -> IO ( G.SessionId, MVar GameState )
fetchGameState maybeSession rState =
  case maybeSession of
    -- Connection request has sessionId
    Just sId -> do
      -- Let's check if it exists in router state
      s <- readMVar rState
      case getSession sId s of
        -- If requested session does not exist
        Nothing -> do
          -- Let's just create it
          newGame <- newMVar Game.newGameState
          -- Update the router state
          modifyMVar_ rState $ \s ->
            return (addSession sId newGame s)
          -- Return new session
          return (sId, newGame)
        -- If session if found we just add it
        Just gState -> return (sId, gState)
    Nothing -> do
      -- User sent a message with no SessionId
      -- We'll need to create a new session
      newGame <- newMVar Game.newGameState
      -- Generate new sessionId
      sId <- genHash
      modifyMVar_ rState $ \s ->
        return (addSession sId newGame s)
      -- Return new session
      return (sId, newGame)

-- Main game backend function
startRouter :: MVar RouterState -> WS.ServerApp
startRouter rState pending = do
  -- Accept connection
  conn <- WS.acceptRequest pending
  -- Check if connection is alive every 30 secs
  WS.forkPingThread conn 30

  -- Let's recieve the initial message
  msg <- WS.receiveData conn :: IO ByteString
  let maybeMsg = decode msg :: Maybe I.Message
  case maybeMsg of
    Nothing -> incorrectMessage msg
    Just initMessage -> do
      case initMessage of
        I.Connect (I.ConnectionData playerName mode maybeSession) -> do

          (sId, gState) <- fetchGameState maybeSession rState
          pId <- genHash

          token <- Game.defaultToken <$> readMVar gState
          
          let player = G.Player pId playerName token
          let client = (player, conn)

          flip finally (disconnect client gState) $ do
            -- Send session context to the new player
            readMVar gState >>= \s -> do
              let moves = G.toCell <$> toList (history s)
                  players = fst <$> clients s
                  ctrlMsg = O.SetSession $
                            O.SessionData sId player players moves
              WS.sendTextData conn (encode ctrlMsg)
            
            -- Add new client to state and notify everybody
            modifyMVar_ gState $ \s -> do
              let s' = Game.addClient client s
              -- Warn everybody
              let ctrlMsg = O.Connected (O.Player player)
              Game.broadcast (encode ctrlMsg) s'
              return s'

            -- Start message exchange
            talk client gState
        _ -> print "Wrong initial message"

-- Message parser and game logic
talk :: Client -> MVar GameState -> IO ()
talk client@(_,conn) state = forever $ do
  -- Recieve a message
  msg <- WS.receiveData conn :: IO ByteString
  -- Try to parse it as contol command
  let maybeMsg = decode msg :: Maybe I.Message
  case maybeMsg of
    Nothing -> incorrectMessage msg
    Just msg -> do
      Game.gameLogic client msg state

-- Disconnect from server
disconnect :: Client -> MVar GameState -> IO ()
disconnect client gState = do
  -- Remove client from state
  s <- modifyMVar gState $ \s ->
    let s' = removeClient client s in return (s',s')

  -- Broadcast that one of the players is disconnected
  let ctrlMsg = O.Disconnected $
                O.Player (fst client)
  broadcast (encode ctrlMsg) s

incorrectMessage :: ByteString -> IO ()
incorrectMessage msg =
  print ("Incorrect message: " <> msg)

genHash :: IO G.Hash
genHash = do
  let hashChr = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
  xs <- sequenceA .
        take 10 .
        repeat $
        randomRIO (0,length hashChr - 1)
  return ((hashChr !!) <$> xs)
