module Cache.Geography where

import qualified Data.Geo.Jord.Angle       as Angle
import qualified Data.Geo.Jord.Geodetic    as Geodesic
import qualified Data.Geo.Jord.GreatCircle as GreatCirlce
import qualified Data.Geo.Jord.Length      as Lenght
import           Types.Wheather            (Coordinates (..))

distance :: Coordinates -> Coordinates -> Double
distance (Coordinates lat1 lon1) (Coordinates lat2 lon2) =
  let point1 = Geodesic.s84Pos lat1 lon1 in
  let point2 = Geodesic.s84Pos lat2 lon2 in
    Lenght.toKilometres $ GreatCirlce.distance point1 point2

getSection :: Coordinates -> Double -> (Double, Double)
getSection (Coordinates lat lon) rangeError =
  let point = Geodesic.s84Pos lat lon in
  let bearingLeft = Angle.decimalDegrees 270 in
  let bearingRight = Angle.decimalDegrees 90 in
  let dist = Lenght.kilometres rangeError in
    let pLeft = Geodesic.decimalLongitude $ GreatCirlce.destination point bearingLeft dist in
    let pRight  = Geodesic.decimalLongitude $ GreatCirlce.destination point bearingRight dist in
    (pLeft, pRight)

