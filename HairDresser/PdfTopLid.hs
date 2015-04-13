import Graphics.PDF

myDocument :: PDF () 
myDocument = do
    page1 <- addPage Nothing
    newSection (toPDFString "Section") Nothing Nothing $ do
     newSection (toPDFString "Subsection") Nothing Nothing $ do
        createPageContent page1

createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = do
	drawWithPage page $ do
		strokeColor red
		setWidth 5
		stroke $ Rectangle (10 :+ 10) (200 :+ 200)
		strokeColor blue
		stroke $ Polygon $ [(40 :+ 30), (90 :+ 130), (140 :+ 30), (40 :+ 30), (90 :+ 130)]

main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
        myDocument
