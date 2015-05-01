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


plotSamplePage :: Draw ()
plotSamplePage = do  
               strokeColor red
               setWidth 1
               plotBeziers jinny
               --beginPath (10 :+ 10)
               --addBezierCubic (30 :+ 100) (100 :+ 100) (150 :+ 10)
               --strokePath
               --stroke (Arc 120.0 120.0 500.0 500.0)

createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = do
	drawWithPage page plotSamplePage 

main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "littorio", compressed = False}) rect $ do
        myDocument
