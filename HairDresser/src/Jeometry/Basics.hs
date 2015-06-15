module Jeometry.Basics
    (
      Point
    , PdfPoint
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
    , angCos
    , angSin
    , angTan
    ) where

import Data.Complex
import Data.Fixed

import Graphics.PDF
import Graphics.PDF.Coordinates (Point, Angle (..), toRadian)

type PdfPoint = Graphics.PDF.Point

data Bezier = Bezier PdfPoint PdfPoint PdfPoint PdfPoint

data CArc = CArc {cArcCenter :: Point, cArcRadius :: Double, cArcStart :: Angle, cArcEnd :: Angle}

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

a ^+ b = Radian (toRadian a + toRadian b)
a ^- b = Radian (toRadian a - toRadian b)
a ^* b = Radian (toRadian a * b)
a ^/ b = Radian (toRadian a / b)
a ^< b = (toRadian a) < (toRadian b)
a ^> b = (toRadian a) > (toRadian b)
angCos = cos . toRadian
angSin = sin . toRadian
angTan = tan . toRadian

