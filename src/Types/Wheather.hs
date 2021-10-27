{-# LANGUAGE DeriveGeneric #-}

module Types.Wheather where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)

data CoordsStr = CoordsStr
  { latStr :: String,
    lonStr :: String
  }
  deriving (Generic, Show)

instance FromJSON CoordsStr

instance ToJSON CoordsStr

data RequestType
  = City String
  | CityId String
  | Coords CoordsStr
  | ZIPCode String
  deriving (Show)

data Coordinates = Coordinates
  { latDoub :: Double,
    lonDoub :: Double
  }
  deriving (Generic)

instance ToJSON Coordinates

instance FromJSON Coordinates