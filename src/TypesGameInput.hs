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
    Connect Data
  | PostMove Data
  | UpdatePlayer Data
  | GetHistory
  | CleanHistory
  deriving (Generic, Eq, Show)
instance FromJSON Message where
  parseJSON = genericParseJSON messageOptions
instance ToJSON Message where
  toJSON = genericToJSON messageOptions

data Data =
    ConnectionData { playerName :: G.PlayerName
                   , mode       :: ConnectionMode
                   , session    :: Maybe G.SessionId }
  | Player G.Player
  | Cell G.Cell
  deriving (Generic, Eq, Show)
instance FromJSON Data where
  parseJSON = genericParseJSON noTagOptions
instance ToJSON Data where
  toJSON = genericToJSON noTagOptions

data ConnectionMode =
    NewGame
  | RandomGame
  deriving (Generic, Eq, Show)
instance FromJSON ConnectionMode
instance ToJSON ConnectionMode
