module CircularArc
    (
      plotArc
    ) where

import Graphics.PDF
import Control.Monad

import Jeometry

type PdfPoint = Graphics.PDF.Point

data Bezier = Bezier PdfPoint PdfPoint PdfPoint PdfPoint

plotOneBezier :: Bezier -> Draw ()
plotOneBezier (Bezier p1 p2 p3 p4) = do
                beginPath p1
                addBezierCubic p2 p3 p4
                strokePath

bezierControlPointsForCenteredCircle :: Double -> Angle -> (PdfPoint, PdfPoint)
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

plotSmallArc :: CArc -> Draw ()
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

-- Takes an angle and breaks it into a series of angles not larger than limit
quantifyAngles :: Angle -> Angle -> [Angle]
quantifyAngles toQuantify = quantifyAngles' [toQuantify] []
    where
        quantifyAngles' (y:ys) x limit = 
            if y ^> limit
              then quantifyAngles' (y^/2 : y^/2 : ys) x limit
              else quantifyAngles' ys (x ++ [y]) limit
        quantifyAngles' [] result _  = result

--turns one Arc into a list of Arcs with size not larger than limit
quantifyArc :: CArc -> Angle -> [CArc]
quantifyArc arc limitAngle = foldl quantifyArc' [] angles where
    startingAngle = cArcStart arc
    angles = quantifyAngles ((cArcEnd arc) ^- (cArcStart arc)) limitAngle
    quantifyArc' prevArcs toAdd 
        | length prevArcs == 0 = [CArc (cArcCenter arc) (cArcRadius arc) startingAngle (startingAngle ^+ toAdd)]
        | otherwise = prevArcs ++ [CArc (cArcCenter arc) (cArcRadius arc) (cArcEnd prevArc) (cArcEnd prevArc ^+ toAdd)]
        where
            prevArc = last prevArcs

plotArc :: CArc -> Draw ()
plotArc arc = mapM_ plotSmallArc (quantifyArc arc quantAngle)
    where quantAngle = (Degree 10)

dumpArc :: CArc -> IO ()
dumpArc arc = do
  print $ "Arc: [Center: " ++ show (cArcCenter arc) ++ "][Radius: " ++ show (cArcRadius arc) ++ "][Start: " ++ show (toDegree (cArcStart arc)) ++ "][End: " ++ show (toDegree (cArcEnd arc)) ++ "]"
