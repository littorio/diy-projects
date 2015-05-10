import Graphics.PDF
import Control.Monad

import Jeometry as J

type PdfPoint = Graphics.PDF.Point

myDocument :: PDF () 
myDocument = do
    page1 <- addPage Nothing
    newSection (toPDFString "Section") Nothing Nothing $ do
     newSection (toPDFString "Subsection") Nothing Nothing $ do
        createPageContent page1


data Bezier = Bezier PdfPoint PdfPoint PdfPoint PdfPoint

plotOneBezier :: Bezier -> Draw ()
plotOneBezier (Bezier p1 p2 p3 p4) = do
                beginPath p1
                addBezierCubic p2 p3 p4
                strokePath

plotBeziers :: [Bezier] -> Draw ()
plotBeziers [] = return ()
plotBeziers (x:xs) = do 
                plotOneBezier x
                plotBeziers xs

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

drawArcByCenterRadiusAndAngles :: PdfPoint -> Angle -> Angle -> Double -> Draw ()
drawArcByCenterRadiusAndAngles (xc :+ yc) startAngle endAngle r = do
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

plotSamplePage :: Draw ()
plotSamplePage = do  
               strokeColor blue
               setWidth 1
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 0) (Degree 30) 70
               --strokeColor green
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 30) (Degree 60) 70
               --strokeColor red
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 60) (Degree 90) 70
               --strokeColor black
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 90) (Degree 120) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 120) (Degree 150) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 150) (Degree 180) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 180) (Degree 210) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 210) (Degree 240) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 240) (Degree 270) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 270) (Degree 300) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 300) (Degree 330) 70
               drawArcByCenterRadiusAndAngles (100 :+ 100) (Degree 330) (Degree 360) 70

createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = do
	drawWithPage page plotSamplePage 

plotSampleDocument :: IO ()
plotSampleDocument = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "littorio", compressed = False}) rect $ do
        myDocument

quantifyAngles :: Angle -> Angle -> [Angle]
quantifyAngles toQuantify = quantifyAngles' [toQuantify] []

quantifyAngles' :: [Angle] -> [Angle] -> Angle -> [Angle]
quantifyAngles' (y:ys) x limit = 
    if y ^> limit
      then quantifyAngles' (y^/2 : y^/2 : ys) x limit
      else quantifyAngles' ys (x ++ [y]) limit
quantifyAngles' [] result _  = result

main :: IO()
main = plotSampleDocument