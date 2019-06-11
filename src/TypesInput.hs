{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TypesInput where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text (toLower)

import qualified TypesGame as Game

data Message = Message {
    mType  :: MessageType
  , mValue :: MessageValue
  } deriving (Generic, Show)
instance FromJSON Message where
  parseJSON (Object v) = Message <$> v.:"type" <*> v.:"value"
  parseJSON _          = empty

data MessageType =
    Command
  | Move
  deriving (Generic, Eq, Show)
instance FromJSON MessageType

data MessageValue =
  -- Command types
    GetHistory
  | CleanGrid
  -- Move type
  | Coordinate Game.Coordinate
  deriving (Generic, Eq, Show)
instance FromJSON MessageValue where
  parseJSON (String s) = case s of
    "GetHistory" -> return GetHistory
    "CleanGrid"  -> return CleanGrid
    _            -> empty
  parseJSON (Object v) = Coordinate <$>
    (Game.Coordinate <$> v.:"row" <*> v.:"col")
  parseJSON _          = empty
instance ToJSON MessageValue
