{-# LANGUAGE DeriveGeneric #-}

module TypesGameOutput where

import Data.Aeson
import Data.Text
import GHC.Generics

import qualified TypesGame as G

-- Structure for output messages
data Message =
    Connected Data
  | Disconnected Data
  | SetSession Data
  | SetCell Data
  | UpdatePlayer Data
  | Win Data
  | Clean
  deriving (Generic, Show)
instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
    { sumEncoding = TaggedObject
      { tagFieldName      = "message"
      , contentsFieldName = "data" }
    }

data Data =
    Player G.Player
  | Cell G.Cell
  | SessionData { session :: G.SessionId
                , me      :: G.Player
                , players :: [G.Player]
                , history :: [G.Cell] }
  deriving (Generic, Show)
instance ToJSON Data where
  toJSON = genericToJSON defaultOptions
    { allNullaryToStringTag = True
    , sumEncoding = ObjectWithSingleField }
