{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.GetByCityId where

import Cache.Getter (getWheather)
import Data.Aeson (Value)
import Data.ByteString.Lazy.Char8 (toStrict, unpack)
import Data.Text (Text)
import Data.Text.Conversions (toText)
import Data.Text.Encoding (decodeUtf8)
import Servant (Get, JSON, QueryParam, type (:>))
import Types.Wheather (RequestType (CityId))
import Utility.Flow (Flow)
import Utility.HttpErrorResponse (httpErrorResponse)

type GetByCityId = "cityid" :> QueryParam "id" String :> Get '[JSON] Value

getByCityId :: Maybe String -> Flow Value
getByCityId Nothing = pure $ httpErrorResponse "Unknown request"
getByCityId (Just cityId) = getWheather (CityId cityId)