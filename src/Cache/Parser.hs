{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cache.Parser where

import Data.Aeson (Object, decode, (.:))
import Data.Aeson.Types (Value (Object), parseMaybe)
import Data.ByteString.Lazy.Char8 (ByteString, pack)

data ResJSON = ResJSON
  { resLat :: Double,
    resLon :: Double,
    time :: Integer
  }

parseWheatherJSON :: Value -> Maybe ResJSON
parseWheatherJSON (Object obj) = do
  coords <- parseMaybe (.: "coord") obj
  resLon <- parseMaybe (.: "lon") coords
  resLat <- parseMaybe (.: "lat") coords
  time <- parseMaybe (.: "dt") obj
  Just $ ResJSON {..}
parseWheatherJSON _ = Nothing