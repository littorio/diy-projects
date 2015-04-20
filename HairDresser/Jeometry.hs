module Jeometry
    (
      Point(..)
    , Shape(..)
    ) where

data Point = Point {getX :: Int, getY :: Int}

data Shape = Line {getLineStart :: Point, getLineEnd :: Point}
			| Arc {getArcCenter :: Point, getArcRadius :: Int, getArcStart :: Int, getArcEnd :: Int}

(#-) :: Int -> Int -> Point
(#-) x y = Point x y




