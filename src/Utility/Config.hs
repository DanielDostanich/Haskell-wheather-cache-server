{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Utility.Config (readConfig) where

import           Control.Exception       (catch)
import           Data.Aeson              (FromJSON, (.:))
import           Data.Maybe              (fromJust, isJust)
import           Data.Text               (Text)
import           Data.Yaml               ((.:?))
import qualified Data.Yaml               as Yaml
import           Types.Config            (ConfigFields (..))
import           Types.Wheather          (CoordsStr (CoordsStr),
                                          RequestType (City, CityId, Coords, ZIPCode))
import           Utility.ConfigException (ConfigExc (getExcText),
                                          throwConfigExc)

readConfig :: IO (Either String ConfigFields)
readConfig =
  catch
    ( do
        eObj <- Yaml.decodeFileEither "config/config.yaml"
        obj <- either (throwConfigExc . Yaml.prettyPrintParseException) pure eObj
        timeError <- readValueDef "timeError" obj 1200
        rangeError <- readValueDef "rangeError" obj 30
        port <- readValue "port" obj
        autoUpdLocations <- readPlacesList obj
        sleepTime <- readValueDef "sleepTime" obj 1800
        pure . Right $ ConfigFields {..}
    )
    (\e -> pure . Left . getExcText $ (e :: ConfigExc))

readValue :: FromJSON a => Text -> Yaml.Object -> IO a
readValue field obj = do
  let eValue = Yaml.parseEither (.: field) obj
  either throwConfigExc pure eValue

readValueDef :: FromJSON a => Text -> Yaml.Object -> a -> IO a
readValueDef field obj def = do
  let eValue = Yaml.parseEither (.: field) obj
  either (pure . const def) pure eValue

readPlacesList :: Yaml.Object -> IO [RequestType]
readPlacesList obj = do
  let lst = Yaml.parseEither (.: "locations") obj
  case lst of
    Left err -> throwConfigExc err
    Right lst_ -> do
      let locations = map (Yaml.parseEither parseLocations) lst_
      mapM helper locations
  where
    helper (Left err)  = throwConfigExc err
    helper (Right loc) = getRequestType loc

    getRequestType :: Location -> IO RequestType
    getRequestType Location {..} = do
      case typ of
        "city" -> maybe (throwConfigExc "Bad location") (pure . City) val
        "cityId" -> maybe (throwConfigExc "Bad location") (pure . CityId) val
        "zipCode" -> maybe (throwConfigExc "Bad location") (pure . ZIPCode) val
        "coords" -> if isJust lat && isJust lon then pure . Coords $ CoordsStr (fromJust lat) (fromJust lon) else throwConfigExc "Bad location"
        _ -> throwConfigExc "Bad location"

data Location = Location
  { typ :: String,
    val :: Maybe String,
    lat :: Maybe String,
    lon :: Maybe String
  }

parseLocations :: Yaml.Object -> Yaml.Parser Location
parseLocations v =
  Location
    <$> v .: "typ"
    <*> v .:? "val"
    <*> v .:? "lat"
    <*> v .:? "lon"
