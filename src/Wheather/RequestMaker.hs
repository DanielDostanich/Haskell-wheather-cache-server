{-# LANGUAGE RecordWildCards #-}

module Wheather.RequestMaker (makeRequest) where

import Data.Text.Conversions (fromText)
import Types.Environment (Environment (wheaterAPIKey))
import Types.Wheather (CoordsStr (..), RequestType (..))
import Utility.Flow (Flow, getEnvironment)

makeRequest :: RequestType -> Flow String
makeRequest reqType = do
  env <- getEnvironment
  let apiKey = wheaterAPIKey env
  let req = makeMainPart reqType
  pure $ req ++ "&appid=" ++ fromText apiKey

address :: String
address = "https://api.openweathermap.org/data/2.5/weather?"

makeMainPart :: RequestType -> String
makeMainPart (City name) = address ++ "q=" ++ name
makeMainPart (CityId cityId) = address ++ "id=" ++ cityId
makeMainPart (Coords CoordsStr {..}) = address ++ "lat=" ++ latStr ++ "&lon=" ++ lonStr
makeMainPart (ZIPCode code) = address ++ "zip=" ++ code