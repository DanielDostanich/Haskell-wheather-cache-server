{-# LANGUAGE OverloadedStrings #-}

module Wheather.Reciever where

import           Data.Aeson.Types      (Value)
import           Network.HTTP.Simple   (getResponseBody, getResponseStatus,
                                        httpJSON, parseRequest)
import           Network.HTTP.Types    (ok200)
import           Types.Wheather        (RequestType (..))
import           Utility.Flow          (Flow)
import           Wheather.RequestMaker (makeRequest)

receiveWheather :: RequestType -> Flow (Either Value Value)
receiveWheather reqType = do
  requestStr <- makeRequest reqType
  request <- parseRequest requestStr
  response <- httpJSON request
  let message = getResponseBody response
  if getResponseStatus response == ok200
    then pure . Right $ message
    else pure . Left $ message
