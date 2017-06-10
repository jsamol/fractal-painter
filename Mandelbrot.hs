module Mandelbrot (drawMandelbrot) where
import Data.Complex
import Graphics.GD
import Plot

maxIteration = 150

drawMandelbrot :: Size -> Bound -> String -> IO()
drawMandelbrot size bound fileName = drawPlot drawMandelbrot_ size bound fileName

drawMandelbrot_ :: Coord -> Color
drawMandelbrot_ (x,y) = rgb (floor $ f * 255) (floor $ f * 120) 0
                        where i = mandelbrot (x :+ y)
                              f = fromIntegral i / fromIntegral maxIteration

mandelbrot :: Complex Double -> Int
mandelbrot z = length $ takeWhile (\c -> magnitude c <= 2) $ take maxIteration $ iterate (\w -> w^2 + z) 0
