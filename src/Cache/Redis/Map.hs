{-# LANGUAGE OverloadedStrings #-}

module Cache.Redis.Map
  ( cityMapAdd,
    cityIDMapAdd,
    zipcodeMapAdd,
    cityMapGet,
    cityIDMapGet,
    zipcodeMapGet,
  )
where

import Cache.Parser (ResJSON (..), parseWheatherJSON)
import Cache.Redis.Queries (mapAdd, mapGet)
import Data.Aeson (Value, decode, encode)
import Types.Wheather (Coordinates (..))
import Utility.Flow (Flow)

cityMapName :: String
cityMapName = "Cities_Coords_Map"

cityIDMapName :: String
cityIDMapName = "CitiesIDs_Coords_Map"

zipcodeMapName :: String
zipcodeMapName = "zipcode_Coords_Map"

cityMapAdd :: String -> Value -> Flow ()
cityMapAdd = mapParseAdd cityMapName

cityIDMapAdd :: String -> Value -> Flow ()
cityIDMapAdd = mapParseAdd cityIDMapName

zipcodeMapAdd :: String -> Value -> Flow ()
zipcodeMapAdd = mapParseAdd zipcodeMapName

cityMapGet :: String -> Flow (Maybe Coordinates)
cityMapGet = mapGetDecode cityMapName

cityIDMapGet :: String -> Flow (Maybe Coordinates)
cityIDMapGet = mapGetDecode cityIDMapName

zipcodeMapGet :: String -> Flow (Maybe Coordinates)
zipcodeMapGet = mapGetDecode zipcodeMapName

mapParseAdd :: String -> String -> Value -> Flow ()
mapParseAdd mapName field json = do
  let mParsed = parseWheatherJSON json
  case mParsed of
    Nothing -> pure ()
    Just parsed -> do
      mapAddEncode mapName field (Coordinates (resLat parsed) (resLon parsed))

mapAddEncode :: String -> String -> Coordinates -> Flow ()
mapAddEncode mapName field coords = do
  let strCoords = encode coords
  mapAdd mapName field strCoords

mapGetDecode :: String -> String -> Flow (Maybe Coordinates)
mapGetDecode mapName field = do
  mCoordsJSON <- mapGet mapName field
  pure $ decode =<< mCoordsJSON