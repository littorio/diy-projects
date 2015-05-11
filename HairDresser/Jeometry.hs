module Jeometry
    (
      Point
    , CArc(..)
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

data CArc = CArc {cArcCenter :: Point, cArcRadius :: Double, cArcStart :: Angle, cArcEnd :: Angle}

toDegree :: Angle -> Double
toDegree (Degree x) = x
toDegree (Radian x) = (x / pi) * 180

normalizeAngle :: Angle -> Angle
normalizeAngle (Degree deg) = Degree (mod' deg 360.0)
normalizeAngle (Radian rad) = Radian (mod' rad (2 * pi))

normalizeArc :: CArc -> CArc
normalizeArc (CArc center radius start end) = CArc center radius (normalizeAngle start) (normalizeAngle end)

arcSize :: CArc -> Angle
arcSize arc = (cArcEnd arc) ^- (cArcStart arc)

a ^+ b = Radian (toRadian a + toRadian b)
a ^- b = Radian (toRadian a - toRadian b)
a ^* b = Radian (toRadian a * b)
a ^/ b = Radian (toRadian a / b)
a ^< b = (toRadian a) < (toRadian b)
a ^> b = (toRadian a) > (toRadian b)
angCos = cos . toRadian
angSin = sin . toRadian
angTan = tan . toRadian

