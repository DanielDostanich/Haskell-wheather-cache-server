{-# LANGUAGE DeriveGeneric #-}

module Utility.HttpErrorResponse where

import Data.Aeson (FromJSON, ToJSON (toJSON), Value)
import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)

data ErrorJSON = ErrorJSON
  { cod :: Int,
    message :: String
  }
  deriving (Generic)

instance FromJSON ErrorJSON

instance ToJSON ErrorJSON

--makes response json for bad argument cases
httpErrorResponse :: String -> Value
httpErrorResponse msg = toJSON $ ErrorJSON 400 msg