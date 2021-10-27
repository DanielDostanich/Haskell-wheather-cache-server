{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cache.Getter (getWheather) where

import Cache.Geography (distance, getSection)
import Cache.Parser
  ( ResJSON (resLat, resLon, time),
    parseWheatherJSON,
  )
import Cache.Redis.Map (cityIDMapAdd, cityIDMapGet, cityMapAdd, cityMapGet, zipcodeMapAdd, zipcodeMapGet)
import Cache.Redis.Set (tempsAdd, tempsRange, tempsRem)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.Bifunctor
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Read (readEither)
import qualified Types.Environment as Environment
import Types.Wheather (Coordinates (..), CoordsStr (..), RequestType (..))
import Utility.Flow (Flow, getEnvironment)
import Utility.HttpErrorResponse (httpErrorResponse)
import Wheather.Reciever (receiveWheather)

type InfoWithJSON = (Value, ResJSON)

getWheather :: RequestType -> Flow Value
getWheather (Coords CoordsStr {..}) = do
  let eLat = readEither latStr :: Either String Double
  let eLon = readEither lonStr :: Either String Double
  case eLat of
    Left msg -> pure . httpErrorResponse $ "lat is not a number"
    Right la ->
      case eLon of
        Left msg -> pure . httpErrorResponse $ "lon is not a number"
        Right lo -> do
          let coords = Coordinates la lo
          getWheatherByCoords coords
getWheather request@(City city) = do
  mCoords <- cityMapGet city
  case mCoords of
    Just coords -> getWheatherByCoords coords
    Nothing -> do
      eRes <- receiveWheather request
      case eRes of
        Left err -> pure err
        Right res -> do
          tempsAdd res
          cityMapAdd city res
          pure res
getWheather request@(CityId cId) = do
  mCoords <- cityIDMapGet cId
  case mCoords of
    Just coords -> getWheatherByCoords coords
    Nothing -> do
      eRes <- receiveWheather request
      case eRes of
        Left err -> pure err
        Right res -> do
          tempsAdd res
          cityIDMapAdd cId res
          pure res
getWheather request@(ZIPCode zipCode) = do
  mCoords <- zipcodeMapGet zipCode
  case mCoords of
    Just coords -> getWheatherByCoords coords
    Nothing -> do
      eRes <- receiveWheather request
      case eRes of
        Left err -> pure err
        Right res -> do
          tempsAdd res
          zipcodeMapAdd zipCode res
          pure res

getWheatherByCoords :: Coordinates -> Flow Value
getWheatherByCoords coords = do
  env <- getEnvironment
  let rangeError = Environment.rangeError env
  let timeError = Environment.timeError env
  let (left, right) = getSection coords rangeError
  let query = if left > right then [(-181, right), (left, 181)] else [(left, right)]
  queryResult <- mapM (uncurry tempsRange) query
  let results = map (\j -> (j, parseWheatherJSON j)) . concat $ queryResult
  temps <- removeInvalid timeError results
  let mFound = findTemp rangeError coords temps
  case mFound of
    Just result -> pure result
    Nothing -> do
      eRes <- receiveWheather (Coords $ CoordsStr (show . latDoub $ coords) (show . lonDoub $ coords))
      either (\l -> pure ()) tempsAdd eRes
      pure $ either id id eRes
  where
    findTemp :: Double -> Coordinates -> [InfoWithJSON] -> Maybe Value
    findTemp rangeError coords lst = do
      (res, _) <- foldr (helper rangeError coords) Nothing lst
      Just res

    helper :: Double -> Coordinates -> InfoWithJSON -> Maybe InfoWithJSON -> Maybe InfoWithJSON
    helper rangeError coordsDef (json, resJSON) mRes =
      let currCoords = Coordinates (resLat resJSON) (resLon resJSON)
       in if distance coordsDef currCoords <= rangeError
            then case mRes of
              Nothing -> Just (json, resJSON)
              Just (accJSON, accResJSON) ->
                if distance coordsDef currCoords
                  < distance coordsDef (Coordinates (resLat accResJSON) (resLon accResJSON))
                  then Just (json, resJSON)
                  else Just (accJSON, accResJSON)
            else mRes

removeInvalid :: Integer -> [(Value, Maybe ResJSON)] -> Flow [InfoWithJSON]
removeInvalid timeError mList = do
  let (mParsed, notParsed) = partition (\(_, mResJSON) -> isJust mResJSON) mList
  let _ = map (\(json, _) -> tempsRem json) notParsed
  let parsed = map (Data.Bifunctor.second fromJust) mParsed
  currentTime <- liftIO (round `fmap` getPOSIXTime)
  let (ok, expired) = partition (\(_, resJSON) -> time resJSON + timeError > currentTime) parsed
  let _ = map (\(json, _) -> tempsRem json) expired
  pure ok
