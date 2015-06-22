module Jeometry.Basics
    (
      module Jeometry.Shapes
    , Bezier (..)
    , (===)
    ) where

import Jeometry.Shapes

data Bezier = Bezier Point Point Point Point


(===) :: Double -> Double -> Bool
0.0 === 0.0 = True
a   === b   = 0.0005 > (abs ((a - b) / a))


