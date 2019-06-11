{-# LANGUAGE OverloadedStrings #-}
module Game where

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
import Data.Aeson (encode, decode)

import qualified TypesGame as Game
import qualified TypesInput as I
import qualified TypesOutput as O

--------------------------------------------------
-- State types
--------------------------------------------------

data ServerState = ServerState {
    clients :: [Client]
  , history :: Game.Grid
  , lastMove :: Game.Coordinate
  }

type Client = (Game.Role, WS.Connection)

--------------------------------------------------
--------------------------------------------------

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

saveMove :: Game.Coordinate -> ServerState -> ServerState
saveMove coord state = ServerState
  (clients state)
  (Map.insert coord value (history state))
  (coord)
  where value = if even (Map.size $ history state)
                then Game.X else Game.O

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
  let user = if null (clients s) then Game.Host else Game.Guest
  let client = (user, conn)
  flip finally (disconnect client state) $ do
    -- Add client to connection list
    modifyMVar_ state $ \s -> do
      let s' = addClient client s
      -- Warn everybody
      let ctrlMsg = O.Message O.User (O.Connected user)
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
  let ctrlMsg = O.Message O.User
                (O.Disconnected $ fst client)
  broadcast (encode ctrlMsg) s

-- Message parser and game logic
talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  -- Recieve a message
  msg <- WS.receiveData conn :: IO ByteString
  -- Try to parse it as contol command
  let maybeMsg = decode msg :: Maybe I.Message
  case maybeMsg of
    Nothing -> print $ ("Incorrect message :" <> msg)
    Just m ->
      case I.mType m of
        I.Command -> case I.mValue m of
          I.GetHistory -> sendHistory (user, conn) state
          I.CleanGrid -> cleanHistory state
        I.Move -> do
          let (I.Coordinate coord) = I.mValue m
          -- Save move into game history
          modifyMVar_ state $ \s -> do
            let s' = saveMove coord s
            return s'
        -- Share the move with other clients
          readMVar state >>= \s -> do
            let value = (history s) Map.! (lastMove s)
            let ctrlMsg = O.Message O.User $
                          O.Move (Game.Cell coord value)
            broadcast (encode ctrlMsg) s
          -- Then check if player won
            if winSituation s
              then let ctrlMsg = O.Message O.User (O.Win user)
                   in broadcast (encode ctrlMsg) s
              else return ()

sendHistory :: Client -> MVar ServerState -> IO ()
sendHistory (user, conn) state = do
  readMVar state >>= \s -> do
    let ctrlMsg = O.Message O.Game (O.History . Game.transformGrid $ history s)
    WS.sendTextData conn (encode ctrlMsg)

cleanHistory :: MVar ServerState -> IO ()
cleanHistory state = do
  modifyMVar_ state $ \s -> return $ ServerState
    (clients s) Map.empty undefined
  readMVar state >>= \s ->
    let ctrlMsg = O.Message O.Game O.Clean
    in broadcast (encode ctrlMsg) s

--------------------------------------------------
-- Analysis functions
--------------------------------------------------

type Direction = (Int, Int)
type Counter = Int

-- Recursively counts symbols in one direction
countStrike :: Game.Coordinate -> Game.Grid
            -> Direction -> Counter
countStrike coord history axis =
  go 0 coord history axis
  where go :: Counter -> Game.Coordinate
           -> Game.Grid -> Direction -> Counter
        go i coord history (deltaR, deltaC) = do
          -- Calculate updated coordinates
          let nextMove = Game.Coordinate
                         (Game.row coord + deltaR)
                         (Game.col coord + deltaC)
          let nextValue = nextMove `Map.lookup` history
          case nextMove `Map.lookup` history of
            Nothing -> i
            Just v -> if v == history Map.! coord
              then go (i + 1) nextMove history (deltaR, deltaC)
              else i

-- Counts symbols in direct and inversed direction
-- Then sums them up and adds 1 (actual recent move)
countStrikes :: Game.Coordinate -> Game.Grid -> [Counter]
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
