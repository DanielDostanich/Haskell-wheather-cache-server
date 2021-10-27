{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.GetByCoords where

import Cache.Getter (getWheather)
import Data.Aeson (Value)
import Data.ByteString.Lazy.Char8 (toStrict, unpack)
import Data.Text (Text)
import Data.Text.Conversions (toText)
import Data.Text.Encoding (decodeUtf8)
import Servant (Get, JSON, QueryParam, type (:>))
import Types.Wheather
  ( CoordsStr (CoordsStr, latStr, lonStr),
    RequestType (Coords),
  )
import Utility.Flow (Flow)
import Utility.HttpErrorResponse (httpErrorResponse)

type GetByCoords = "coords" :> QueryParam "lat" String :> QueryParam "lon" String :> Get '[JSON] Value

getByCoords :: Maybe String -> Maybe String -> Flow Value
getByCoords Nothing _ = pure $ httpErrorResponse "Unknown request"
getByCoords _ Nothing = pure $ httpErrorResponse "Unknown request"
getByCoords (Just latStr) (Just lonStr) = getWheather (Coords $ CoordsStr {..})
