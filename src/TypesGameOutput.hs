{-# LANGUAGE DeriveGeneric #-}

module TypesGameOutput where

import Data.Aeson
import Data.Text
import GHC.Generics

import qualified TypesGame as TG

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
  -- User values
    Connected TG.Player
  | Disconnected TG.Player
  | Move TG.Cell
  | Win TG.Player
  -- Game values
  | NewSession [Char]
  | History [TG.Cell]
  | Clean
  deriving (Generic, Show)
instance ToJSON MessageValue
