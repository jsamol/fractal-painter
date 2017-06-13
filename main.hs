{-|
Module      : main
Description : Main

Module which invoke function.
-}
module Main where
import Mandelbrot
import Plot
import Graphics.GD

-- |Name of generating file
fileName = "mandelbrot.png"

-- |Main function
main:: IO()
main = putStrLn "Welcome in program, invoke function 'draw' to draw fractal"

-- |Invoke function to draw Mandelbrot fractal
draw :: Size -- ^ Resolution of generating image
        -> Bound -- ^ Four coordinates of funtion bound
        -> IO() -- ^ Result: Save file
draw size bound = drawMandelbrot size bound fileName
