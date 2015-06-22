import Graphics.PDF
import Control.Monad

import qualified Jeometry as J

type PdfPoint = Graphics.PDF.Point

myDocument :: PDF () 
myDocument = do
    page1 <- addPage Nothing
    newSection (toPDFString "Section") Nothing Nothing $ do
     newSection (toPDFString "Subsection") Nothing Nothing $ do
        createPageContent page1


createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = do
	drawWithPage page plotSampleArc 

plotSampleDocument :: IO ()
plotSampleDocument = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "littorio", compressed = False}) rect $ do
        myDocument

sampleArc1 = J.CArc (J.PlanarAngle (100 J.:+ 100) (J.Degree 0) (J.Degree 90)) 70
sampleArc2 = J.CArc (J.PlanarAngle (100 J.:+ 100) (J.Degree 0) (J.Degree 120)) 120

plotSampleArc :: Draw ()
plotSampleArc = do
    strokeColor blue
    setWidth 1
    J.plotArc sampleArc1
    strokeColor red
    J.plotArc sampleArc2

main :: IO()
main = plotSampleDocument