module Jeometry.CircularArc
    (
      splitAngle
    , splitArc
    , dumpArc
    ) where

import Graphics.PDF
import Control.Monad

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
        splitArc' smallAngle count 
            | count == 0 = []
            | otherwise = [CArc (cArcCenter arc) (cArcRadius arc) startAngle endAngle] ++ splitArc' smallAngle (count-1)
                where
                    startAngle = (cArcStart arc) ^+ (smallAngle ^* (count - 1))
                    endAngle = (cArcStart arc) ^+ (smallAngle ^* count)

dumpArc :: CArc -> IO ()
dumpArc arc = do
  print $ "Arc: [Center: " ++ show (cArcCenter arc) ++ "][Radius: " ++ show (cArcRadius arc) ++ "][Start: " ++ show (toDegree (cArcStart arc)) ++ "][End: " ++ show (toDegree (cArcEnd arc)) ++ "]"
