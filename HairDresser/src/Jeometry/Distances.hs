module Jeometry.Distances (DistanceToPoint (..)) where

import Jeometry.Basics

class DistanceToPoint a where
    distToPoint :: a -> Point -> Double

instance DistanceToPoint Point where
    distToPoint (x1 :+ y1) (x2 :+ y2) = sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))

