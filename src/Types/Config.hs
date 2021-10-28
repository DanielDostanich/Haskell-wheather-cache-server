module Types.Config (ConfigFields (..)) where

import           GHC.Base       (Double, Int)
import           GHC.Num        (Integer)
import           Prelude        (Show)
import           Types.Wheather (RequestType)

data ConfigFields = ConfigFields
  { timeError        :: Integer,
    rangeError       :: Double,
    port             :: Int,
    autoUpdLocations :: [RequestType],
    sleepTime        :: Int
  }
  deriving (Show)
