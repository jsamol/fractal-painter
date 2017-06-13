{-|
Module      : Mandelbrot
Description : Algorithm of Mandelbrot functions

Module which get resolution of image and points and calculate
correct color to point based on Mandelbrot algorithm
-}
module Mandelbrot (drawMandelbrot) where
import Data.Complex
import Graphics.GD
import Plot

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
