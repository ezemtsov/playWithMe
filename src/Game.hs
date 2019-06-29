{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.HashMap.Strict.InsOrd as HM
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
  }

type Client = (TG.Player, WS.Connection)

--------------------------------------------------
--------------------------------------------------

newGameState :: GameState
newGameState = GameState [] HM.empty

numClients :: GameState -> Int
numClients = length . clients

clientExists :: Client -> GameState -> Bool
clientExists client = any ((== fst client) . fst) . clients

addClient :: Client -> GameState -> GameState
addClient client state = GameState
  (client : clients state)
  (history state)

removeClient :: Client -> GameState -> GameState
removeClient client state = GameState
  (filter ((/= fst client) . fst) $ clients state)
  (history state)

broadcast :: ByteString -> GameState -> IO ()
broadcast message state = do
  forM_ (clients state) $ \(_, conn) ->
    WS.sendTextData conn message

addMove :: TG.Cell -> GameState -> GameState
addMove (TG.Cell coord value) state =
  GameState
  (clients state)
  (HM.insert coord value (history state))

lastMove :: GameState -> TG.Cell
lastMove state = let (c, v) = head $
                       HM.toRevList (history state)
                 in TG.Cell c v

--------------------------------------------------
-- Game server functions
--------------------------------------------------

type GameAction = MVar GameState -> IO ()

-- Function that encapsulates message processing logic
gameLogic :: Client -> I.Message -> GameAction
gameLogic client msg state = case msg of
  I.Get d -> case d of
    I.History -> sendHistory client state
  I.Delete d -> case d of
    I.History -> cleanHistory state
  I.Post d -> case d of
    I.Move cell -> saveMove client cell state

saveMove :: Client -> TG.Cell -> GameAction
saveMove (user, conn) cell state = do
  -- Save move into game history
  modifyMVar_ state $ \s -> do
    let s' = addMove cell s
    return s'
  -- Share the move with other clients
  readMVar state >>= \s -> do
    let ctrlMsg = O.Message O.User (O.Move cell)
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
                  ( O.History ( TG.toCell <$> HM.toList (history s)
                              , fst <$> clients s ))
    WS.sendTextData conn (encode ctrlMsg)

cleanHistory :: GameAction
cleanHistory state = do
  modifyMVar_ state $ \s -> return $
    GameState (clients s) HM.empty
  readMVar state >>= \s ->
    let ctrlMsg = O.Message O.Game O.Clean
    in broadcast (encode ctrlMsg) s

--------------------------------------------------
-- Analysis functions
--------------------------------------------------

type Direction = (Int, Int)
type Counter = Int

-- Recursively counts symbols in one direction
countStrike :: TG.Cell -> TG.Grid
            -> Direction -> Counter
countStrike cell history axis =
  go 0 cell history axis
  where go :: Counter -> TG.Cell
           -> TG.Grid -> Direction -> Counter
        go i (TG.Cell coord cellValue)
          history (deltaR, deltaC) = do
          -- Calculate updated coordinates
          let nextMove = TG.Coordinate
                         (TG.row coord + deltaR)
                         (TG.col coord + deltaC)
          let nextValue = HM.lookup nextMove history
          case nextValue of
            Nothing -> i
            Just v -> if v == cellValue
              then go (i + 1) (TG.Cell nextMove v)
                   history (deltaR, deltaC)
              else i

-- Counts symbols in direct and inversed direction
-- Then sums them up and adds 1 (actual recent move)
countStrikes :: TG.Cell -> TG.Grid -> [Counter]
countStrikes cell history =
  let count = map (countStrike cell history)
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
