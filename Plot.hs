{-|
Module      : Plot
Description : Draw the image

Module which change resolution to array of points
and draw the image based on colors from Mandelbrot
-}
module Plot
    ( Coord
    , Bound
    , drawPlot
   , runPlotTests
   , runQuickCheckPlot
    ) where
import Graphics.GD
import Test.HUnit
import Test.QuickCheck

-- |Type of coordinate point
type Coord = (Double ,Double) -- ^ X, Y coordinate

-- |Type of bound the function
type Bound = (Coord ,Coord) -- ^ Left top and right bottom bound

-- |Draw image
drawPlot :: (Coord -> Color) -- ^ Function converting a point to the appropriate color
            -> Size -- ^ Resolution of generating image
            -> Bound -- ^ Four coordinates of funtion bound
            -> String -- ^ Name of generating file
            -> IO() -- ^ Result: Save file
drawPlot f size bound fileName = do
    image <- newImage size 
    mapM (\p -> let coords = pixelToCoord p size bound in setPixel p (f coords) image) (getPixels size)  
    savePngFile fileName image

-- |Draw points from resolution 
getPixels :: Size -- ^ Resolution of generating image
            -> [Point] -- ^ List of all points
getPixels (width, height) = [(x,y) | x <- [0..(width - 1)], y <- [0..(height - 1)]]

-- |Scaling point to function's coordinates
pixelToCoord :: Point -- ^ Point from resolution
                -> Size -- ^ Resolution of generating image
                -> Bound -- ^ Four coordinates of funtion bound
                -> Coord -- ^Correct coordinates
pixelToCoord (x,y) (xsize, ysize) ((x1,y1), (x2,y2)) =
    ((((x2 - x1) * xscale) + x1), (((y1 - y2) * yscale) + y2))
        where xscale = fromIntegral x / fromIntegral xsize
              yscale = fromIntegral y / fromIntegral ysize

test1 = TestCase (assertEqual "pixels1" [(0,0), (0,1), (1,0), (1,1)] (getPixels (2,2)))
test2 = TestCase (assertEqual "pixels2" [] (getPixels (0,0)))
test3 = TestCase (assertEqual "coord1"  (100.0,100.0) (pixelToCoord (0,0) (1, 1) ((100,100), (100,100))))
test4 = TestCase (assertEqual "coord2"  (0.0,1.0) (pixelToCoord (100,100) (1000, 1000) ((0,1), (0,1))))
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4]

-- |Run tests
runPlotTests :: IO Counts
runPlotTests = do runTestTT tests

pixels  :: Size -> Bool
pixels (x,y) =  (toBool $ map (< (x,y)) $ getPixels (x,y)) == True

toBool     ::  [Bool] -> Bool
toBool []              = True
toBool (x:xs)  
    | x == False        = False
    | otherwise         = toBool xs

-- |Run quick tests
runQuickCheckPlot = do quickCheck pixels 
