{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TypesGameInput where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

import qualified TypesGame as G

messageOptions = defaultOptions
  { sumEncoding = TaggedObject
    { tagFieldName      = "method"
    , contentsFieldName = "resource" }
  }

noTagOptions = defaultOptions
  { sumEncoding = ObjectWithSingleField }

data Message =
    Connect ConnectionData
  | PostMove G.Cell
  | UpdatePlayer G.Player
  | GetHistory
  | CleanHistory
  deriving (Generic, Eq, Show)
instance FromJSON Message where
  parseJSON = genericParseJSON messageOptions
instance ToJSON Message where
  toJSON = genericToJSON messageOptions

data ConnectionData = ConnectionData
  { playerName :: G.PlayerName
  , mode       :: ConnectionMode
  , session    :: Maybe G.SessionId
  } deriving (Generic, Eq, Show)
instance FromJSON ConnectionData
instance ToJSON ConnectionData

data ConnectionMode =
    NewGame
  | RandomGame
  deriving (Generic, Eq, Show)
instance FromJSON ConnectionMode
instance ToJSON ConnectionMode
