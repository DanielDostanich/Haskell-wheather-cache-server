{-# LANGUAGE OverloadedStrings #-}

module Cache.Redis.Set (tempsAdd, tempsRange, tempsRem) where

import Cache.Parser (ResJSON (..), parseWheatherJSON)
import Cache.Redis.Queries (setAdd, setGetRange, setRem)
import Data.Aeson (Value)
import Utility.Flow (Flow)

setName :: String
setName = "Temperature_Ordered_Set"

tempsAdd :: Value -> Flow ()
tempsAdd json = do
  let mParsed = parseWheatherJSON json
  case mParsed of
    Nothing -> pure ()
    Just parsed -> do
      setAdd setName (resLon parsed) json

tempsRange :: Double -> Double -> Flow [Value]
tempsRange = setGetRange setName

tempsRem :: Value -> Flow ()
tempsRem = setRem setName