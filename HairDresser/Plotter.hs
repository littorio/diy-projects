import Graphics.PDF

--import Jeometry as J

myDocument :: PDF () 
myDocument = do
    page1 <- addPage Nothing
    newSection (toPDFString "Section") Nothing Nothing $ do
     newSection (toPDFString "Subsection") Nothing Nothing $ do
        createPageContent page1


plotPage :: Draw ()
plotPage = do  
               strokeColor red
               setWidth 1
               beginPath (10 :+ 10)
               addBezierCubic (30 :+ 100) (100 :+ 100) (150 :+ 10)
               strokePath
               stroke (Arc 120.0 120.0 500.0 500.0)

createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = do
	drawWithPage page plotPage 

main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "littorio", compressed = False}) rect $ do
        myDocument
