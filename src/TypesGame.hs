{-# LANGUAGE DeriveGeneric #-}
module TypesGame where

import qualified Data.Map.Strict as Map

import Data.Aeson
import GHC.Generics

data Role = Host | Guest
  deriving (Generic, Eq, Show)
instance FromJSON Role
instance ToJSON Role

type Grid = Map.Map Coordinate CellValue

data Cell = Cell {
    coord :: Coordinate
  , value :: CellValue
  } deriving (Generic, Eq, Show)
instance FromJSON Cell
instance ToJSON Cell

-- We recieve pure coordinates from front
data Coordinate = Coordinate {
    row :: Int
  , col :: Int
  } deriving (Generic, Eq, Show, Ord)
instance FromJSON Coordinate
instance ToJSON Coordinate
instance ToJSONKey Coordinate

data CellValue = X | O
  deriving (Generic, Eq, Show)
instance ToJSON CellValue
instance FromJSON CellValue

transformGrid :: Grid -> [Cell]
transformGrid grid = map toCell (Map.toList grid)
  where toCell (k, v) = Cell k v
