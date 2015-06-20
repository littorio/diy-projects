module Jeometry.CircularArc
    (
      splitAngle
    , splitArc
    , dumpArc
    , bezierControlPointsForCenteredCircle
    ) where

import Jeometry.Basics

splitAngle :: Angle -> Angle -> (Angle, Double)
splitAngle src limit = (divisor, count)
    where
        count = fromIntegral (ceiling (angleRatio src limit))
        divisor = src ^/ count

-- splits arc into a list of arcs of given angle
splitArc :: CArc -> Angle -> [CArc]
splitArc arc limit = splitArc' smallAngle count
    where
        (smallAngle, count) = splitAngle (arcSize arc) limit
        splitArc' angle numOfAngles 
            | count == 0 = []
            | otherwise = [CArc (cArcCenter arc) (cArcRadius arc) startAngle endAngle] ++ splitArc' angle (numOfAngles-1)
                where
                    startAngle = (cArcStart arc) ^+ (angle ^* (numOfAngles - 1))
                    endAngle = (cArcStart arc) ^+ (angle ^* numOfAngles)

bezierControlPointsForCenteredCircle :: Double -> Angle -> (Point, Point)
bezierControlPointsForCenteredCircle r arcAngle = ((x2 :+ y2), (x3 :+ y3))
  where
    a = arcAngle ^/ 2
-- This one is suggestedby internet, but it is less pleasant for my eye    let k = 0.5522847498
    k = 0.6522847498
    x4 = r * angCos a
    y4 = r * angSin a
    x1 = x4
    y1 = -y4
    f = k * angTan a
    x2 = x1 + f * y4
    y2 = y1 + f * x4
    x3 = x2
    y3 = -y2

cArcPointAtAngle :: CArc -> Angle -> Point
cArcPointAtAngle arc angle = ((centerX + radius * (angCos angle)) :+ (centerY + radius * (angSin angle))) where
  centerX = (pointX (cArcCenter arc))
  centerY = (pointY (cArcCenter arc))
  radius = (cArcRadius arc)

--cArcFirstPoint :: CArc -> Point
--cArcFirstPoint (CArc center radius startAngle endAngle) = ((pointX center) + radius * (angCos startAngle) :+ (pointY center) + radius * (angSin startAngle))

--cArcLastPoint :: CArc -> Point
--cArcLastPoint (CArc center radius startAngle endAngle) = ((pointX center) + radius * (angCos endAngle) :+ 
                                                          --(pointY center) + radius * (angSin endAngle))

dumpArc :: CArc -> IO ()
dumpArc arc = do
  print $ "Arc: [Center: " ++ show (cArcCenter arc) ++ "][Radius: " ++ show (cArcRadius arc) ++ "][Start: " ++ show (toDegree (cArcStart arc)) ++ "][End: " ++ show (toDegree (cArcEnd arc)) ++ "]"

