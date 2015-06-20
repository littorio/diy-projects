module Jeometry.Basics
    (
      Point (..)
    , pointX
    , pointY
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
    , angACos
    , PlanarAngle (..)
    , CenterVector
    , cVectorFrom2Points
    , normalizeCVector
    , pAngleValue
    , planarAngleCenterRay
    , planarAngleFromCArc
    , dotProductOf2CVectors
    ) where

import Data.Fixed

data Point = Double :+ Double deriving (Show, Eq)

pointX :: Point -> Double
pointX (x :+ _) = x

pointY :: Point -> Double
pointY (_ :+ y) = y

data Bezier = Bezier Point Point Point Point

data CArc = CArc {cArcCenter :: Point, cArcRadius :: Double, cArcStart :: Angle, cArcEnd :: Angle}

(===) :: Double -> Double -> Bool
0.0 === 0.0 = True
a   === b   = 0.0005 > (abs ((a - b) / a))

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

angACos :: Angle -> Double
angACos = acos . toRadian

type CenterVector = (Double, Double)

cVectorFrom2Points :: Point -> Point -> CenterVector
cVectorFrom2Points (x1 :+ y1) (x2 :+ y2) = ((x2 - x1), (y2 - y1))

normalizeCVector :: CenterVector -> CenterVector
normalizeCVector (srcX, srcY) = (srcX / srcLength, srcY / srcLength)
  where srcLength = sqrt $ (srcX ** 2) + (srcY ** 2)

dotProductOf2CVectors :: CenterVector -> CenterVector -> Double
dotProductOf2CVectors (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

data PlanarAngle = PlanarAngle {pAngleCenter :: Point, pAngleStart :: Angle, pAngleEnd :: Angle}

pAngleValue :: PlanarAngle -> Angle
pAngleValue planarAngle = (pAngleEnd planarAngle)  ^- (pAngleStart planarAngle)

planarAngleCenterRay :: PlanarAngle -> CenterVector
planarAngleCenterRay pAngle = ((angCos angle), (angSin angle))
  where angle = (pAngleStart pAngle) ^+ ((pAngleValue pAngle) ^/ 2)

planarAngleFromCArc :: CArc -> PlanarAngle
planarAngleFromCArc a = PlanarAngle (cArcCenter a) (cArcStart a) (cArcEnd a)

