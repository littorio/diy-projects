import Graphics.PDF
import Graphics.PDF.Coordinates


--let point = Point 0 15

myDocument :: PDF () 
myDocument = do
    page1 <- addPage Nothing
    newSection (toPDFString "Section") Nothing Nothing $ do
     newSection (toPDFString "Subsection") Nothing Nothing $ do
        createPageContent page1

--jinnyDraw :: Draw ()
--jinnyDraw =  

--createPageContent :: PDFReference PDFPage -> Draw () 
createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = do
	drawWithPage page $ do
		strokeColor red
		setWidth 5
		-- stroke $ Rectangle 0 (200 :+ 100)
		-- stroke $ Rectangle 0 (300 :+ 50)
		--strokeColor (Rgb 200 0 200)
		stroke $ Rectangle (10 :+ 10) (200 :+ 200)
		strokeColor blue
		stroke $ Polygon $ [(40 :+ 30), (90 :+ 130), (140 :+ 30), (40 :+ 30), (90 :+ 130)]

-- drawWithPage page $ do
		--return PDF () 
--      fillColor blue
--      fill $ Ellipse 100 100 300 200	
--    strokeColor red
--    setWidth 0.5
--    stroke $ Rectangle (Point 10 0) (Point 200 300)

--tuper :: Point
--tuper = (100 :+ 150)

main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
        myDocument
--main = do
--	print $ show $ realPart $ tuper