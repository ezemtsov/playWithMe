{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Map.Strict as Map
import Data.ByteString.Lazy (ByteString)

import Control.Monad (forM_, forever)
import Control.Concurrent
import Control.Exception (finally)

import qualified Network.WebSockets as WS
import Data.Aeson (encode, decode)

import qualified TypesGame as TG
import qualified TypesGameInput as I
import qualified TypesGameOutput as O

--------------------------------------------------
-- State types
--------------------------------------------------

data GameState = GameState {
    clients :: [Client]
  , history :: TG.Grid
  , lastMove :: TG.Coordinate
  }

type Client = (TG.Player, WS.Connection)

--------------------------------------------------
--------------------------------------------------

newGameState :: GameState
newGameState = GameState [] Map.empty undefined

numClients :: GameState -> Int
numClients = length . clients

clientExists :: Client -> GameState -> Bool
clientExists client = any ((== fst client) . fst) . clients

addClient :: Client -> GameState -> GameState
addClient client state = GameState
  (client : clients state)
  (history state)
  (lastMove state)

removeClient :: Client -> GameState -> GameState
removeClient client state = GameState
  (filter ((/= fst client) . fst) $ clients state)
  (history state)
  (lastMove state)

broadcast :: ByteString -> GameState -> IO ()
broadcast message state = do
  forM_ (clients state) $ \(_, conn) ->
    WS.sendTextData conn message

addMove :: TG.Coordinate -> GameState -> GameState
addMove coord state = GameState
  (clients state)
  (Map.insert coord value (history state))
  (coord)
  where value = if even (Map.size $ history state)
                then TG.X else TG.O

--------------------------------------------------
-- Game server functions
--------------------------------------------------

type GameAction = MVar GameState -> IO ()

-- Function that encapsulates message processing logic
gameLogic :: Client -> I.Message -> GameAction
gameLogic client msg state = case msg of
  I.Get d -> case d of
    I.History -> do
      sendHistory client state
  I.Delete d -> case d of
    I.History -> cleanHistory state
  I.Post d -> case d of
    I.Move coord -> saveMove client coord state

saveMove :: Client -> TG.Coordinate -> GameAction
saveMove (user, conn) coord state = do
  -- Save move into game history
  modifyMVar_ state $ \s -> do
    let s' = addMove coord s
    return s'
  -- Share the move with other clients
  readMVar state >>= \s -> do
    let value = (history s) Map.! (lastMove s)
    let ctrlMsg = O.Message O.User $
                  O.Move (TG.Cell coord value)
    broadcast (encode ctrlMsg) s
      -- Then check if player won
    if winSituation s
      then let ctrlMsg = O.Message O.User (O.Win user)
           in broadcast (encode ctrlMsg) s
      else return ()

sendHistory :: Client -> GameAction
sendHistory (user, conn) state = do
  readMVar state >>= \s -> do
    let ctrlMsg = O.Message O.Game
                  ( O.History ( TG.transformGrid $ history s
                              , fst <$> clients s ))
    WS.sendTextData conn (encode ctrlMsg)

cleanHistory :: GameAction
cleanHistory state = do
  modifyMVar_ state $ \s -> return $ GameState
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
countStrike :: TG.Coordinate -> TG.Grid
            -> Direction -> Counter
countStrike coord history axis =
  go 0 coord history axis
  where go :: Counter -> TG.Coordinate
           -> TG.Grid -> Direction -> Counter
        go i coord history (deltaR, deltaC) = do
          -- Calculate updated coordinates
          let nextMove = TG.Coordinate
                         (TG.row coord + deltaR)
                         (TG.col coord + deltaC)
          let nextValue = nextMove `Map.lookup` history
          case nextMove `Map.lookup` history of
            Nothing -> i
            Just v -> if v == history Map.! coord
              then go (i + 1) nextMove history (deltaR, deltaC)
              else i

-- Counts symbols in direct and inversed direction
-- Then sums them up and adds 1 (actual recent move)
countStrikes :: TG.Coordinate -> TG.Grid -> [Counter]
countStrikes move history =
  let count = map (countStrike move history)
      zipWithSum = zipWith (fmap (+1) . (+))
  in zipWithSum (count right) (count left)
  where right = [(-1,0),(-1,-1),(0,-1),(1,-1)]
        left  = [(1,0),(1,1),(0,1),(-1,1)]

-- Checks if last event lead to win situation
winSituation :: GameState -> Bool
winSituation state =
  -- If 5 or more, player won
  5 <= (maximum $ countStrikes fromMove (history state))
  -- We only check last move
  where fromMove = (lastMove state)
