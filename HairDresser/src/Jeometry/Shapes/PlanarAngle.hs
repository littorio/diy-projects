module Jeometry.Shapes.PlanarAngle
    (
      PlanarAngle (..)	
    , pAngleValue
    , pAngleCenterVector
    ) where

import Jeometry.Shapes.Primitives
import Jeometry.Shapes.Vector

data PlanarAngle = PlanarAngle {pAngleCenter :: Point, pAngleStart :: Angle, pAngleEnd :: Angle}

pAngleValue :: PlanarAngle -> Angle
pAngleValue planarAngle = (pAngleEnd planarAngle)  ^- (pAngleStart planarAngle)

pAngleCenterVector :: PlanarAngle -> Vector
pAngleCenterVector pAngle = ((angCos angle) :- (angSin angle))
  where angle = (pAngleStart pAngle) ^+ ((pAngleValue pAngle) ^/ 2)

