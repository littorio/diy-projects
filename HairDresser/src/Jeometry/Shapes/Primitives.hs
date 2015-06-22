module Jeometry.Shapes.Primitives
    (
      Point (..)
    , pointX
    , pointY
    , Angle (..)
    , toDegree
    , toRadian
    , angleRatio
    , normalizeAngle
    , (^+)
    , (^-)
    , (^*)
    , (^/)
    , (^<)
    , (^>)
    , angCos
    , angSin
    , angTan
    , angACos
    ) where

import Data.Fixed

data Point = Double :+ Double deriving (Show, Eq)

pointX :: Point -> Double
pointX (x :+ _) = x

pointY :: Point -> Double
pointY (_ :+ y) = y

data Angle = Degree !Double -- ^ Angle in degrees
           | Radian !Double -- ^ Angle in radians
           deriving (Show)

toRadian :: Angle -> Double
toRadian (Degree x) = (pi / 180) * x
toRadian (Radian x) = x

toDegree :: Angle -> Double
toDegree (Degree x) = x
toDegree (Radian x) = (x / pi) * 180

angleRatio :: Angle -> Angle -> Double
angleRatio a b = (toDegree a) / (toDegree b)

normalizeAngle :: Angle -> Angle
normalizeAngle (Degree deg) = Degree (mod' deg 360.0)
normalizeAngle (Radian rad) = Radian (mod' rad (2 * pi))

(^+) :: Angle -> Angle -> Angle
a ^+ b = Radian (toRadian a + toRadian b)

(^-) :: Angle -> Angle -> Angle
a ^- b = Radian (toRadian a - toRadian b)

(^*) :: Angle -> Double -> Angle
a ^* b = Radian (toRadian a * b)

(^/) :: Angle -> Double -> Angle
a ^/ b = Radian (toRadian a / b)

(^<) :: Angle -> Angle -> Bool
a ^< b = (toRadian a) < (toRadian b)

(^>) :: Angle -> Angle -> Bool
a ^> b = (toRadian a) > (toRadian b)

angCos :: Angle -> Double
angCos = cos . toRadian

angSin :: Angle -> Double
angSin = sin . toRadian

angTan :: Angle -> Double
angTan = tan . toRadian

angACos :: Angle -> Double
angACos = acos . toRadian