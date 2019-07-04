{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.HashMap.Strict.InsOrd as HM
import Data.List ((!!))
import Data.ByteString.Lazy (ByteString)

import Control.Monad (forM_, forever)
import Control.Concurrent
import Control.Exception (finally)

import qualified Network.WebSockets as WS
import Data.Aeson (encode, decode)

import qualified TypesGame as G
import qualified TypesGameInput as I
import qualified TypesGameOutput as O

--------------------------------------------------
-- Game State
--------------------------------------------------

data GameState = GameState {
    clients :: [Client]
  , history :: G.Grid
  }

type Client = (G.Player, WS.Connection)

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
  (filter ((/= G.id (fst client)) . G.id . fst) $ clients state)
  (history state)

broadcast :: ByteString -> GameState -> IO ()
broadcast message state = do
  forM_ (clients state) $ \(_, conn) ->
    WS.sendTextData conn message

addMove :: G.Cell -> GameState -> GameState
addMove (G.Cell coord value) state =
  GameState
  (clients state)
  (HM.insert coord value (history state))

lastMove :: GameState -> G.Cell
lastMove state = let (c, v) = head $
                       HM.toRevList (history state)
                 in G.Cell c v

updatePlayer :: Client -> G.Player
            -> GameState -> GameState
updatePlayer client@(oldPlayer,conn) newPlayer state =
  addClient newClient . removeClient client $ state
  where newClient = (newPlayer,conn)

defaultToken :: GameState -> G.PlayerToken
defaultToken state = G.Token code color
  where code = ["X","O"] !! ((numClients state) `mod` 2)
        color = "black"

--------------------------------------------------
-- Game server functions
--------------------------------------------------

type GameAction = MVar GameState -> IO ()

-- Function that encapsulates message processing logic
gameLogic :: Client -> I.Message -> GameAction
gameLogic client msg state = case msg of
  I.PostMove (I.Cell cell) -> postMoveAction client cell state
  I.CleanHistory -> cleanHistoryAction state
  I.UpdatePlayer (I.Player player) -> updatePlayerAction client player state
  _ -> print $ "Handler is not implemented: " <> show msg

updatePlayerAction :: Client -> G.Player -> GameAction
updatePlayerAction client newPlayer state = do
  -- Update player with new token
  modifyMVar_ state $ \s -> do
    let s' = updatePlayer client newPlayer s
    return s'
  -- Share update with other clients
  readMVar state >>= \s -> do
    let ctrlMsg = O.UpdatePlayer (O.Player newPlayer)
    broadcast (encode ctrlMsg) s

postMoveAction :: Client -> G.Cell -> GameAction
postMoveAction (player, conn) cell state = do
  -- Save move into game history
  modifyMVar_ state $ \s -> do
    let s' = addMove cell s
    return s'
  -- Share the move with other clients
  readMVar state >>= \s -> do
    let ctrlMsg = O.SetCell (O.Cell cell)
    broadcast (encode ctrlMsg) s
      -- Then check if player won
    if winSituation s
      then let ctrlMsg = O.Win (O.Player player)
           in broadcast (encode ctrlMsg) s
      else return ()

cleanHistoryAction :: GameAction
cleanHistoryAction state = do
  modifyMVar_ state $ \s -> return $
    GameState (clients s) HM.empty
  readMVar state >>= \s ->
    let ctrlMsg = O.Clean
    in broadcast (encode ctrlMsg) s

--------------------------------------------------
-- Analysis functions
--------------------------------------------------

type Direction = (Int, Int)
type Counter = Int

-- Recursively counts symbols in one direction
countStrike :: G.Cell -> G.Grid
            -> Direction -> Counter
countStrike cell history axis =
  go 0 cell history axis
  where go :: Counter -> G.Cell
           -> G.Grid -> Direction -> Counter
        go i (G.Cell coord cellValue)
          history (deltaR, deltaC) = do
          -- Calculate updated coordinates
          let nextMove = G.Coordinate
                         (G.x coord + deltaR)
                         (G.y coord + deltaC)
          let nextValue = HM.lookup nextMove history
          case nextValue of
            Nothing -> i
            Just v -> if v == cellValue
              then go (i + 1) (G.Cell nextMove v)
                   history (deltaR, deltaC)
              else i

-- Counts symbols in direct and inversed direction
-- Then sums them up and adds 1 (actual recent move)
countStrikes :: G.Cell -> G.Grid -> [Counter]
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
