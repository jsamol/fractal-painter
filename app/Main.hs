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

-- |Name of the generated file
fileName = "mandelbrot.png"

-- |Dimensions of the generated image
imageDimensions :: Size
imageDimensions = (600, 600)

-- |Drawing area for the Mandelbrot Set
drawingArea :: Bound
drawingArea = ((-1.9, 1.3), (0.7, -1.3))

-- |Main function
main:: IO()
main = drawMandelbrot imageDimensions drawingArea fileName

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
