{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Endpoints.GetByCity where

import           Cache.Getter              (getWheather)
import           Data.Aeson                (Value)
import           Servant                   (Get, JSON, QueryParam, type (:>))
import           Types.Wheather            (RequestType (City))
import           Utility.Flow              (Flow)
import           Utility.HttpErrorResponse (httpErrorResponse)

type GetByCity = "city" :> QueryParam "q" String :> Get '[JSON] Value

getByCity :: Maybe String -> Flow Value
getByCity Nothing     = pure $ httpErrorResponse "Unknown rfdequest"
getByCity (Just city) = getWheather (City city)
