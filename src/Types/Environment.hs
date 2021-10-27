module Types.Environment
  ( Environment (..),
  )
where

import Data.Text (Text)
import Database.Redis (Connection)
import Types.Wheather (RequestType)

data Environment = Environment
  { wheaterAPIKey :: Text,
    timeError :: Integer,
    rangeError :: Double,
    port :: Int,
    autoUpdLocations :: [RequestType],
    sleepTime :: Int,
    conn :: Connection
  }