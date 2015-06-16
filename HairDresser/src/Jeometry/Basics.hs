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
    , (===)
    , angCos
    , angSin
    , angTan
    , distBetween2Points
    ) where

import Data.Complex
import Data.Fixed

import Graphics.PDF

type PdfPoint = Graphics.PDF.Point

data Bezier = Bezier PdfPoint PdfPoint PdfPoint PdfPoint

data CArc = CArc {cArcCenter :: Point, cArcRadius :: Double, cArcStart :: Angle, cArcEnd :: Angle}

(===) :: Double -> Double -> Bool
0.0 === 0.0 = True
a   === b   = 0.0005 > (abs ((a - b) / a))

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

(^*) :: Angle -> PDFFloat -> Angle
a ^* b = Radian (toRadian a * b)

(^/) :: Angle -> PDFFloat -> Angle
a ^/ b = Radian (toRadian a / b)

(^<) :: Angle -> Angle -> Bool
a ^< b = (toRadian a) < (toRadian b)

(^>) :: Angle -> Angle -> Bool
a ^> b = (toRadian a) > (toRadian b)

angCos :: Angle -> PDFFloat
angCos = cos . toRadian

angSin :: Angle -> PDFFloat
angSin = sin . toRadian

angTan :: Angle -> PDFFloat
angTan = tan . toRadian

distBetween2Points :: PdfPoint -> PdfPoint -> Double
distBetween2Points (x1 :+ y1) (x2 :+ y2) = sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))

