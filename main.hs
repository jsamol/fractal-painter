module Main where
import Mandelbrot
import Plot
import Graphics.GD

fileName = "mandelbrot.png"

draw :: Size -> Bound -> IO()
draw size bound = drawMandelbrot size bound fileName
