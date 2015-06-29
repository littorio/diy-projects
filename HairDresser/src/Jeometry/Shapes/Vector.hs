module Jeometry.Shapes.Vector
    (
      Vector (..)
    , vectorFrom2Points
    , normalizeVector
    , dotProductOf2Vectors
    , pointByOffset
    , invertVector
    ) where

import Jeometry.Shapes.Primitives

data Vector = Double :- Double deriving (Show, Eq)

vectorFrom2Points :: Point -> Point -> Vector
vectorFrom2Points (x1 :+ y1) (x2 :+ y2) = ((x2 - x1) :- (y2 - y1))

normalizeVector :: Vector -> Vector
normalizeVector (x :- y) = ((x / vectorLength) :- (y / vectorLength))
  where vectorLength = sqrt $ (x ** 2) + (y ** 2)

invertVector :: Vector -> Vector
invertVector (x :- y) = ((0-x) :- (0-y))

dotProductOf2Vectors :: Vector -> Vector -> Double
dotProductOf2Vectors (x1 :- y1) (x2 :- y2) = x1 * x2 + y1 * y2

pointByOffset :: Point -> Vector -> Point
pointByOffset (x :+ y) (offX :- offY) = ((x + offX) :+ (y + offY))


