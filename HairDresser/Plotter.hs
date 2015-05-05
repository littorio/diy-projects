import Graphics.PDF
import Control.Monad

--import Jeometry as J

myDocument :: PDF () 
myDocument = do
    page1 <- addPage Nothing
    newSection (toPDFString "Section") Nothing Nothing $ do
     newSection (toPDFString "Subsection") Nothing Nothing $ do
        createPageContent page1


newtype Bezier = Bezier (Point, Point, Point, Point)

plotOneBezier :: Bezier -> Draw ()
plotOneBezier (Bezier (p1, p2, p3, p4)) = do
                beginPath p1
                addBezierCubic p2 p3 p4
                strokePath

jinny :: [Bezier]
jinny = [ (Bezier ((10 :+ 10), (12 :+ 12), (300 :+ 300), (500 :+ 100)))
          , (Bezier ((100 :+ 100), (120 :+ 120), (13 :+ 300), (500 :+ 10)))
          , (Bezier ((10 :+ 10), (200 :+ 12), (400 :+ 400), (22 :+ 310)))]


plotBeziers :: [Bezier] -> Draw ()
plotBeziers [] = return ()
plotBeziers (x:xs) = do 
                plotOneBezier x
                plotBeziers xs

drawArcBy3Points :: Point -> Point -> Point -> Draw ()
drawArcBy3Points (x1 :+ y1) (xc :+ yc) (x4 :+ y4) = do
            let ax = xc - x1
            let ay = yc - y1
            let bx = xc - x4
            let by = yc - y4
            let q1 = ax * ax + ay * ay
            let q2 = q1 + ax * bx + ay * by
            let k2 = 4 / 3 * (sqrt(2 * q1 * q2) - q2) / (ax * by - ay*bx)
            let x2 = xc + ax - k2*ay
            let y2 = yc + ay + k2*ax
            let x3 = xc + bx + k2*by
            let y3 = yc + by - k2*bx
            plotOneBezier (Bezier ((x1:+y1), (x2:+y2), (x3:+y3), (x4:+y4)))

drawArcByCenterRadiusAndAngles2 :: Point -> Angle -> Angle -> Double -> Draw ()
drawArcByCenterRadiusAndAngles2 (xc :+ yc) startAngle endAngle r = do
            let a1 = toRadian startAngle
            let a2 = toRadian endAngle
            let a = (a2 - a1) / 2
-- This one is suggestedby internet, but it is less pleasant for my eye    let k = 0.5522847498
            let k = 0.6522847498
            let x4 = r * cos(a)
            let y4 = r * sin (a)
            let x1 = x4
            let y1 = -y4
            let f = k * (tan a)
            let x2 = x1 + f * y4
            let y2 = y1 + f * x4
            let x3 = x2
            let y3 = -y2
            let ar = a + a1
            let cos_ar = cos(ar)
            let sin_ar = sin(ar)

            let xf1 = xc + r * (cos a1)
            let yf1 = yc + r * (sin a1)
            let xf2 = xc + x2 * cos_ar - y2 * sin_ar
            let yf2 = yc + x2 * sin_ar + y2 * cos_ar
            let xf3 = xc + x3 * cos_ar - y3 * sin_ar
            let yf3 = yc + x3 * sin_ar + y3 * cos_ar
            let xf4 = xc + r * (cos a2)
            let yf4 = yc + r * (sin a2)
            plotOneBezier (Bezier ((xf1:+yf1), (xf2:+yf2), (xf3:+yf3), (xf4:+yf4)))

drawArcByCenterRadiusAndAngles :: Point -> Angle -> Angle -> Double -> Draw ()
drawArcByCenterRadiusAndAngles (xc :+ yc) a b r = do
            let aRadians = toRadian a
            let bRadians = toRadian b
            let xm = xc + r * sin (aRadians + bRadians / 2)
            let ym = yc + r * cos (aRadians + bRadians / 2)
            let x1 = xc + r * sin (aRadians)
            let y1 = yc + r * cos (aRadians)
            let x4 = xc + r * sin (aRadians + bRadians)
            let y4 = yc + r * cos (aRadians + bRadians)
            beginPath (x1 :+ y1)
            lineto (xm:+ym)
            lineto (x4:+y4)
            strokePath
            drawArcBy3Points (x1 :+ y1) (xm :+ ym) (x4 :+ y4)

plotSamplePage :: Draw ()
plotSamplePage = do  
               strokeColor blue
               setWidth 1
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 0) (Degree 30) 70
               --strokeColor green
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 30) (Degree 60) 70
               --strokeColor red
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 60) (Degree 90) 70
               --strokeColor black
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 90) (Degree 120) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 120) (Degree 150) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 150) (Degree 180) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 180) (Degree 210) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 210) (Degree 240) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 240) (Degree 270) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 270) (Degree 300) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 300) (Degree 330) 70
               drawArcByCenterRadiusAndAngles2 (100 :+ 100) (Degree 330) (Degree 360) 70

               strokeColor red
               plotBeziers jinny

createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = do
	drawWithPage page plotSamplePage 

main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "littorio", compressed = False}) rect $ do
        myDocument
