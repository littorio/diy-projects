{-# LANGUAGE FlexibleInstances #-}

module Jeometry.Basics
    (
      Point
    , Angle (..)
    , Bezier (..)
    , CArc(..)
    , normalizeArc
    , toDegree
    , arcSize
    , angleRatio
    , (^+)
    , (^-)
    , (^*)
    , (^/)
    , (^<)
    , (^>)
    , (===)
    , angCos
    , angSin
    , angTan
    , Shape (..)
    ) where

import Data.Complex
import Data.Fixed

type Point = Complex Double

data Bezier = Bezier Point Point Point Point

data CArc = CArc {cArcCenter :: Point, cArcRadius :: Double, cArcStart :: Angle, cArcEnd :: Angle}

(===) :: Double -> Double -> Bool
0.0 === 0.0 = True
a   === b   = 0.0005 > (abs ((a - b) / a))

data Angle = Degree !Double -- ^ Angle in degrees
           | Radian !Double -- ^ Angle in radians

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

normalizeArc :: CArc -> CArc
normalizeArc (CArc center radius start end) = CArc center radius (normalizeAngle start) (normalizeAngle end)

arcSize :: CArc -> Angle
arcSize arc = (cArcEnd arc) ^- (cArcStart arc)

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

class Shape a where
    distToPoint :: a -> Point -> Double

instance Shape Point where
    distToPoint (x1 :+ y1) (x2 :+ y2) = sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))
