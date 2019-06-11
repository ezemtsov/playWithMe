{-# LANGUAGE DeriveGeneric #-}
module TypesOutput where

import Data.Aeson
import GHC.Generics

import qualified TypesGame as Game

-- Structure for output messages
data Message = Message {
    mType  :: MessageType
  , mValue :: MessageValue
  } deriving (Generic, Show)
instance ToJSON Message

data MessageType =
    User
  | Game
  deriving (Generic, Eq, Show)
instance ToJSON MessageType

data MessageValue =
    Connected Game.Role
  | Disconnected Game.Role
  | Move Game.Cell
  | Win Game.Role
  | History [Game.Cell]
  | Clean
  deriving (Generic, Show)
instance ToJSON MessageValue
