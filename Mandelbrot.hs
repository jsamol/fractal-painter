module Mandelbrot (drawMandelbrot) where
import Data.Complex
import Graphics.GD
import Plot

maxIteration = 1000

drawMandelbrot :: Size -> String -> IO()
drawMandelbrot size fileName = drawPlot drawMandelbrot_ size fileName

drawMandelbrot_ :: Coordinate -> Color
drawMandelbrot_ (x,y) = if m < 2
                        then rgb 255 255 255
                        else rgb 0 0 0
                        where m = magnitude $ mandelbrot (x :+ y)

mandelbrot :: Complex Double -> Complex Double
mandelbrot z = iterate (\w -> w^2 + z) 0 !! maxIteration
