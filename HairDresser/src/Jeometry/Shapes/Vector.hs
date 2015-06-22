module Jeometry.Shapes.Vector
    (
      Vector (..)
    , vectorFrom2Points
    , normalizeVector
    , dotProductOf2Vectors
    ) where

import Jeometry.Shapes.Primitives

data Vector = Double :- Double deriving (Show, Eq)

vectorFrom2Points :: Point -> Point -> Vector
vectorFrom2Points (x1 :+ y1) (x2 :+ y2) = ((x2 - x1) :- (y2 - y1))

normalizeVector :: Vector -> Vector
normalizeVector (x :- y) = ((x / vectorLength) :- (y / vectorLength))
  where vectorLength = sqrt $ (x ** 2) + (y ** 2)

dotProductOf2Vectors :: Vector -> Vector -> Double
dotProductOf2Vectors (x1 :- y1) (x2 :- y2) = x1 * x2 + y1 * y2



