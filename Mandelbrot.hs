{-|
Module      : Mandelbrot
Description : Algorithm of Mandelbrot functions

Module which get resolution of image and points and calculate
correct color to point based on Mandelbrot algorithm
-}
module Mandelbrot (drawMandelbrot, runMandelbrotTests, runQuickCheckMandelbrot) where
import Data.Complex
import Graphics.GD
import Plot
import Test.HUnit
import Test.QuickCheck

-- |Max number of iteration to approximate set
maxIteration = 150

-- |Main function, invoke draw Mandelbrot set
drawMandelbrot :: Size -- ^ Resolution of generating image
                -> Bound -- ^ Four coordinates of funtion bound
                -> String -- ^ Name of generating file
                -> IO() -- ^ Result: Save file
drawMandelbrot size bound fileName = drawPlot drawMandelbrot_ size bound fileName

-- |Generating colors for points
drawMandelbrot_ :: Coord -- ^ Point to draw 
                -> Color -- ^ Color of the point calculated from mandelbrot formula
drawMandelbrot_ (x,y) = rgb (floor $ f * 255) (floor $ f * 120) 0
                        where i = mandelbrot (x :+ y)
                              f = fromIntegral i / fromIntegral maxIteration
                              
-- |Recursive formula for points
mandelbrot :: Complex Double -- ^ Checking point
              -> Int -- ^ Number of iteration
mandelbrot z = length $ takeWhile (\c -> magnitude c <= 2) $ take maxIteration $ iterate (\w -> w^2 + z) 0

test1 = TestCase (assertEqual "color1" 16742400 (drawMandelbrot_ (0,0)))
test2 = TestCase (assertEqual "color2" 196864 (drawMandelbrot_ (1,1)))
test3 = TestCase (assertEqual "color3" 65536 (drawMandelbrot_ (10,10)))
test4 = TestCase (assertEqual "mandelbrot1" 2 (mandelbrot (1 :+1)))
test5 = TestCase (assertEqual "mandelbrot2" 1 (mandelbrot (0 :+ 20)))
test6 = TestCase (assertEqual "mandelbrot3" 1 (mandelbrot (123 :+ 0)))
test7 = TestCase (assertEqual "mandelbrot3" 150 (mandelbrot (0 :+ 0)))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7]

-- |Run tests
runMandelbrotTests :: IO Counts
runMandelbrotTests = do runTestTT tests

max_Color :: Coord -> Bool
max_Color (x,y) =   drawMandelbrot_ (x,y)  <= 16777216

min_Color :: Coord -> Bool
min_Color (x,y) =   drawMandelbrot_ (x,y)  >= 0

-- |Run quick tests
runQuickCheckMandelbrot = do quickCheck max_Color >> quickCheck min_Color