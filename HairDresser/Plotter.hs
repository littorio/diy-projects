import Graphics.PDF
import Control.Monad

import Jeometry
import CircularArc

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

sampleArc = CArc (100 :+ 100) 70 (Degree 10) (Degree 300)

plotSampleArc :: Draw ()
plotSampleArc = do
    strokeColor blue
    setWidth 1
    plotArc sampleArc

main :: IO()
main = plotSampleDocument