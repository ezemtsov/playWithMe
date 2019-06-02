{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import GHC.Generics
import Data.Aeson
import Data.Tuple
import qualified Data.Map.Strict as Map

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (forM_, forever)
import Control.Concurrent
import Control.Exception (finally)

import qualified Network.WebSockets as WS

--------------------------------------------------
-- Game state and related functions
--------------------------------------------------

data ServerState = ServerState {
    clients :: [Client]
  , history :: Grid
  , lastMove :: Coordinate
  }

type Client = (Role, WS.Connection)

data Role = Host | Guest
  deriving (Generic, Eq, Show)
instance ToJSON Role

type Grid = Map.Map Coordinate CellValue

-- We recieve pure coordinates from front
data Coordinate = Coordinate {
    row :: Int
  , col :: Int
  } deriving (Generic, Eq, Show, Ord)
instance FromJSON Coordinate where
instance ToJSON Coordinate

data MsgType = Connected
             | Disconnected
             | Move
             | Win
  deriving (Generic, Eq, Show)
instance ToJSON MsgType

data CellValue = X | O
  deriving (Generic, Eq, Show)
instance ToJSON CellValue

data MsgValue = User Role
              | Cell { coord :: Coordinate
                     , value :: CellValue }
  deriving (Generic, Show)
instance ToJSON MsgValue

-- Structure for control messages
data ControlMsg = ControlMsg {
    msgType :: MsgType
  , msgValue :: MsgValue
  } deriving (Generic, Show)
instance ToJSON ControlMsg

newServerState :: ServerState
newServerState = ServerState [] Map.empty undefined

numClients :: ServerState -> Int
numClients = length . clients

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst) . clients

addClient :: Client -> ServerState -> ServerState
addClient client state = ServerState
  (client : clients state)
  (history state)
  (lastMove state)

removeClient :: Client -> ServerState -> ServerState
removeClient client state = ServerState
  (filter ((/= fst client) . fst) $ clients state)
  (history state)
  (lastMove state)

broadcast :: ByteString -> ServerState -> IO ()
broadcast message state = do
  forM_ (clients state) $ \(_, conn) ->
    WS.sendTextData conn message

saveMove :: Coordinate -> ServerState -> ServerState
saveMove coord state = ServerState
  (clients state)
  (Map.insert coord value (history state))
  (coord)
  where value = if even (Map.size $ history state)
                then X else O

--------------------------------------------------
-- Game server functions
--------------------------------------------------

-- Main game backend function
gameServer :: MVar ServerState -> WS.ServerApp
gameServer state pending = do
  -- Accept connection
  conn <- WS.acceptRequest pending
  -- Check if connection is alive every 30 secs
  WS.forkPingThread conn 30
  -- Recieve an initial message
  msg <- WS.receiveData conn :: IO ByteString

  s <- readMVar state
  -- First client is host
  let user = if null (clients s) then Host else Guest
  let client = (user, conn)
  flip finally (disconnect client state) $ do
    -- Add client to connection list
    modifyMVar_ state $ \s -> do
      let s' = addClient client s
      -- Warn everybody
      let ctrlMsg = ControlMsg Connected (User user)
      broadcast (encode ctrlMsg) s'
      return s'
    -- Start message exchange
    talk client state

-- Disconnect from server
disconnect :: Client -> MVar ServerState -> IO ()
disconnect client state = do
  -- Remove client from state
  s <- modifyMVar state $ \s ->
    let s' = removeClient client s in return (s',s')
  -- Broadcast that one of the players is disconnected
  let ctrlMsg = ControlMsg Disconnected (User $ fst client)
  broadcast (encode ctrlMsg) s

-- Message parser and game logic
talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  -- Recieve a message
  msg <- WS.receiveData conn :: IO ByteString
  -- Try to parse it as move
  let maybeCoord = decode msg :: Maybe Coordinate
  case maybeCoord of
    -- When wrong format print alert
    Nothing -> print $ "Bad message: " <> msg
    -- When correct format process event
    Just coord -> do
      -- Save move into game history
      modifyMVar_ state $ \s -> do
        let s' = saveMove coord s
        return s'
      -- Share the move with other clients
      readMVar state >>= \s -> do
        let value = (history s) Map.! (lastMove s)
        let ctrlMsg = ControlMsg Move (Cell coord value)
        broadcast (encode ctrlMsg) s
        -- Then check if player won
        if winSituation s
          then let ctrlMsg = ControlMsg Win (User user)
               in broadcast (encode ctrlMsg) s
          else return ()

--------------------------------------------------
-- Analysis functions
--------------------------------------------------

type Direction = (Int, Int)
type Counter = Int

-- Recursively counts symbols in one direction
countStrike :: Coordinate -> Grid
            -> Direction -> Counter
countStrike coord history axis =
  go 0 coord history axis
  where go :: Counter -> Coordinate
           -> Grid -> Direction -> Counter
        go i coord history (deltaR, deltaC) = do
          -- Calculate updated coordinates
          let nextMove = Coordinate (row coord + deltaR) (col coord + deltaC)
          let nextValue = nextMove `Map.lookup` history
          case nextMove `Map.lookup` history of
            Nothing -> i
            Just v -> if v == history Map.! coord
              then go (i + 1) nextMove history (deltaR, deltaC)
              else i

-- Counts symbols in direct and inversed direction
-- Then sums them up and adds 1 (actual recent move)
countStrikes :: Coordinate -> Grid -> [Counter]
countStrikes move history =
  let count = map (countStrike move history)
      zipWithSum = zipWith (fmap (+1) . (+))
  in zipWithSum (count right) (count left)
  where right = [(-1,0),(-1,-1),(0,-1),(1,-1)]
        left  = [(1,0),(1,1),(0,1),(-1,1)]

-- Checks if last event lead to win situation
winSituation :: ServerState -> Bool
winSituation state =
  -- If 5 or more, player won
  5 <= (maximum $ countStrikes fromMove (history state))
  -- We only check last move
  where fromMove = (lastMove state)
