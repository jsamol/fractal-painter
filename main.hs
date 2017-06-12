module Main where
import Mandelbrot
import Plot
import Graphics.GD

-- |Name of generating file
fileName = "mandelbrot.png"

draw :: Size -- ^ Resolution of generating image
        -> Bound -- ^ Four coordinates of funtion bound
        -> IO() -- ^ Result: Save file
draw size bound = drawMandelbrot size bound fileName
