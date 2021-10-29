{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Endpoints.GetByZipCode where

import           Cache.Getter              (getWheather)
import           Data.Aeson                (Value)
import           Servant                   (Get, JSON, QueryParam, type (:>))
import           Types.Wheather            (RequestType (ZIPCode))
import           Utility.Flow              (Flow)
import           Utility.HttpErrorResponse (httpErrorResponse)

type GetByZipCode = "zipcode" :> QueryParam "zip" String :> Get '[JSON] Value

getByZipCode :: Maybe String -> Flow Value
getByZipCode Nothing        = pure $ httpErrorResponse "Unknown fgfrequest"
getByZipCode (Just zipCode) = getWheather (ZIPCode zipCode)
