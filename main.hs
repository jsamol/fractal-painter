module Main where
import Mandelbrot

fileName = "mandelbrot.png"

draw :: IO ()
draw = do
    x <- getLine
    y <- getLine
    drawMandelbrot (read x :: Int, read y :: Int) fileName
