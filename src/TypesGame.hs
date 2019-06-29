{-# LANGUAGE DeriveGeneric #-}
module TypesGame where

import qualified Data.HashMap.Strict.InsOrd as HM
import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Data.Hashable

type Player = Text
type Grid = HM.InsOrdHashMap Coordinate CellValue

data Cell = Cell {
    coord :: Coordinate
  , value :: CellValue
  } deriving (Generic, Eq, Show)
instance FromJSON Cell
instance ToJSON Cell

data Coordinate = Coordinate {
    row :: Int
  , col :: Int
  } deriving (Generic, Eq, Show, Ord)
instance FromJSON Coordinate
instance ToJSON Coordinate
instance ToJSONKey Coordinate
instance Hashable Coordinate

type CellValue = Text

toCell :: (Coordinate, CellValue) -> Cell
toCell (k,v) = Cell k v
