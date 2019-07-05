{-# LANGUAGE DeriveGeneric #-}

module TypesGameOutput where

import Data.Aeson
import Data.Text
import GHC.Generics

import qualified TypesGame as G

-- Structure for output messages
data Message =
    Connected G.Player
  | Disconnected G.Player
  | UpdatePlayer G.Player
  | SetSession SessionData
  | SetCell G.Cell
  | Win G.Player
  | Clean
  deriving (Generic, Show)
instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
    { sumEncoding = TaggedObject
      { tagFieldName      = "message"
      , contentsFieldName = "data" }
    }

data SessionData = SessionData
  { session :: G.SessionId
  , me      :: G.Player
  , players :: [G.Player]
  , history :: [G.Cell]
  } deriving (Generic, Show)
instance ToJSON SessionData
