{-# LANGUAGE DeriveGeneric #-}

module TypesGameOutput where

import Data.Aeson
import Data.Text
import GHC.Generics

import qualified TypesGame as TG

-- Structure for output messages
data Message =
    Connected Data
  | Disconnected Data
  | Win Data
  | SetSession Data
  | SetHistory Data
  | Move Data
  | Clean
  deriving (Generic, Show)
instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
    { sumEncoding = TaggedObject
      { tagFieldName      = "message"
      , contentsFieldName = "data" }
    }

data Data =
    Player TG.Player
  | Cell TG.Cell
  | SessionId TG.SessionId
  | History { moves   :: [TG.Cell]
            , players :: [TG.Player] }
  deriving (Generic, Show)
instance ToJSON Data where
  toJSON = genericToJSON defaultOptions
    { allNullaryToStringTag = True
    , sumEncoding = ObjectWithSingleField }
