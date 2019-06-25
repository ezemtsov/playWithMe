{-# LANGUAGE DeriveGeneric #-}
module TypesGame where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Aeson
import GHC.Generics

type Player = Text
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
