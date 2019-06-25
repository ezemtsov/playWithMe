{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TypesGameInput where

import Data.Aeson
import GHC.Generics

import qualified TypesGame as TG

data Message =
    Connect Data
  | Get Data
  | Post Data
  | Delete Data
  deriving (Generic, Eq, Show)
instance FromJSON Message
instance ToJSON Message

data Data =
    Player TG.Player
  | Move TG.Coordinate
  | History
  deriving (Generic, Eq, Show)
instance FromJSON Data
instance ToJSON Data
