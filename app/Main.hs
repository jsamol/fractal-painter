{-|
Module      : main
Description : Main

Module which invoke function.
-}
module Main where
import Mandelbrot
import Plot
import Graphics.GD
import Test.HUnit

-- |Name of generating file
fileName = "mandelbrot.png"

-- |Main function
main:: IO()
main = draw (1200, 1200) ((-1.9, 1.3), (0.7, -1.3))

-- |Invoke function to draw Mandelbrot fractal
draw :: Size -- ^ Resolution of generating image
        -> Bound -- ^ Four coordinates of funtion bound
        -> IO() -- ^ Result: Save file
draw size bound = drawMandelbrot size bound fileName

-- |Run all tests
runTests:: IO Counts
runTests = do
                runMandelbrotTests
                runPlotTests

-- |Run all quick tests
runQuickTests:: IO ()
runQuickTests = do
                runQuickCheckMandelbrot
                runQuickCheckPlot
