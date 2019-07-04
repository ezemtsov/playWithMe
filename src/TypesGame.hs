{-# LANGUAGE DeriveGeneric #-}
module TypesGame where

import qualified Data.HashMap.Strict.InsOrd as HM
import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Data.Hashable

type Hash = [Char]

type SessionId = Hash

data Player = Player {
    id    :: PlayerId
  , name  :: PlayerName
  , token :: PlayerToken
  } deriving (Generic, Eq, Show)
instance FromJSON Player
instance ToJSON Player

type PlayerId = Hash
type PlayerName = Text

data PlayerToken = Token {
    code  :: Text
  , color :: Text
  } deriving (Generic, Eq, Show)
instance FromJSON PlayerToken
instance ToJSON PlayerToken

type Grid = HM.InsOrdHashMap Coordinate PlayerToken

data Cell = Cell {
    coord :: Coordinate
  , value :: PlayerToken
  } deriving (Generic, Eq, Show)
instance FromJSON Cell
instance ToJSON Cell

data Coordinate = Coordinate {
    x :: Int
  , y :: Int
  } deriving (Generic, Eq, Show, Ord)
instance FromJSON Coordinate
instance ToJSON Coordinate
instance ToJSONKey Coordinate
instance Hashable Coordinate


toCell :: (Coordinate, PlayerToken) -> Cell
toCell (k,v) = Cell k v
