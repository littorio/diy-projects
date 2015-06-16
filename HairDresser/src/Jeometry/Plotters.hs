module Jeometry.Plotters
    (
      plotArc
    , plotOneBezier
    ) where

import qualified Graphics.PDF as PDF
import Control.Monad

import Jeometry.Basics
import Jeometry.CircularArc

toPdfPoint :: Point -> PDF.Point
toPdfPoint (x :+ y) = (x PDF.:+ y)

plotOneBezier :: Bezier -> PDF.Draw ()
plotOneBezier (Bezier p1 p2 p3 p4) = do
                PDF.beginPath (toPdfPoint p1)
                PDF.addBezierCubic (toPdfPoint p2) (toPdfPoint p3) (toPdfPoint p4)
                PDF.strokePath

plotSmallArc :: CArc -> PDF.Draw ()
plotSmallArc arc = do
    let startAngle = cArcStart arc
    let endAngle = cArcEnd arc
    let r = cArcRadius arc
    let (xc :+ yc) = cArcCenter arc

    let ((x2 :+ y2), (x3 :+ y3)) = bezierControlPointsForCenteredCircle r (endAngle ^- startAngle)
    let a = (endAngle ^- startAngle) ^/ 2

    let ar = a ^+ startAngle
    let cos_ar = angCos ar
    let sin_ar = angSin ar

    let xf1 = xc + r * angCos startAngle
    let yf1 = yc + r * angSin startAngle
    let xf2 = xc + x2 * cos_ar - y2 * sin_ar
    let yf2 = yc + x2 * sin_ar + y2 * cos_ar
    let xf3 = xc + x3 * cos_ar - y3 * sin_ar
    let yf3 = yc + x3 * sin_ar + y3 * cos_ar
    let xf4 = xc + r * angCos endAngle
    let yf4 = yc + r * angSin endAngle
    plotOneBezier (Bezier (xf1:+yf1) (xf2:+yf2) (xf3:+yf3) (xf4:+yf4))

plotArc :: CArc -> PDF.Draw ()
plotArc arc = mapM_ plotSmallArc (splitArc arc splitAngle)
    where splitAngle = (Degree 10)

dumpArc :: CArc -> IO ()
dumpArc arc = do
  print $ "Arc: [Center: " ++ show (cArcCenter arc) ++ "][Radius: " ++ show (cArcRadius arc) ++ "][Start: " ++ show (toDegree (cArcStart arc)) ++ "][End: " ++ show (toDegree (cArcEnd arc)) ++ "]"
