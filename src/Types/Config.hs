module Types.Config (ConfigFields (..)) where

import Data.Text (Text)
import GHC.Base (Double, Int)
import GHC.Num (Integer)
import Types.Wheather (RequestType)
import Prelude (Show)

data ConfigFields = ConfigFields
  { timeError :: Integer,
    rangeError :: Double,
    port :: Int,
    autoUpdLocations :: [RequestType],
    sleepTime :: Int
  }
  deriving (Show)