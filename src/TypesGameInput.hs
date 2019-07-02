{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TypesGameInput where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

import qualified TypesGame as TG

messageOptions = defaultOptions
  { sumEncoding = TaggedObject
    { tagFieldName = "method"
    , contentsFieldName = "resource" }
  }

resourceOptions = defaultOptions
  { sumEncoding = ObjectWithSingleField }

data Message =
    Connect Data
  | PostMove Data
  | GetHistory
  | CleanHistory
  deriving (Generic, Eq, Show)
instance FromJSON Message where
  parseJSON = genericParseJSON messageOptions
instance ToJSON Message where
  toJSON = genericToJSON messageOptions

data Data =
    Player TG.Player
  | Cell TG.Cell
  deriving (Generic, Eq, Show)
instance FromJSON Data where
  parseJSON = genericParseJSON resourceOptions
instance ToJSON Data where
  toJSON = genericToJSON resourceOptions
