module Jeometry
    (
      Point
    , Arc(..)
    , normalizeArc
    , toDegree
    , (^+)
    , (^-)
    , (^*)
    , (^/)
    , (^<)
    , (^>)
    , angCos
    , angSin
    , angTan
    ) where

import Data.Complex
import Graphics.PDF.Coordinates (Point, Angle (..), toRadian)
import Data.Fixed

data Arc = Arc {arcCenter :: Point, arcRadius :: Double, arcStart :: Angle, arcEnd :: Angle}

toDegree :: Angle -> Double
toDegree (Degree x) = x
toDegree (Radian x) = (x / pi) * 180

normalizeAngle :: Angle -> Angle
normalizeAngle (Degree deg) = Degree (mod' deg 360.0)
normalizeAngle (Radian rad) = Radian (mod' rad (2 * pi))

normalizeArc :: Arc -> Arc
normalizeArc (Arc center radius start end) = Arc center radius (normalizeAngle start) (normalizeAngle end)

arcSize :: Arc -> Angle
arcSize arc = (arcEnd arc) ^- (arcStart arc)

a ^+ b = Radian (toRadian a + toRadian b)
a ^- b = Radian (toRadian a - toRadian b)
a ^* b = Radian (toRadian a * b)
a ^/ b = Radian (toRadian a / b)
a ^< b = (toRadian a) < (toRadian b)
a ^> b = (toRadian a) > (toRadian b)
angCos = cos . toRadian
angSin = sin . toRadian
angTan = tan . toRadian

