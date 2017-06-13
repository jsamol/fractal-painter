{-|
Module      : Plot
Description : Draw the image

Module which change resolution to array of points
and draw the image based on colors from Mandelbrot
-}
module Plot
    ( Coord
    , Bound
    , drawPlot
    ) where
import Graphics.GD

-- |Type of coordinate point
type Coord = (Double ,Double) -- ^ X, Y coordinate

-- |Type of bound the function
type Bound = (Coord ,Coord) -- ^ Left top and right bottom bound

-- |Draw image
drawPlot :: (Coord -> Color) -- ^ Function converting a point to the appropriate color
            -> Size -- ^ Resolution of generating image
            -> Bound -- ^ Four coordinates of funtion bound
            -> String -- ^ Name of generating file
            -> IO() -- ^ Result: Save file
drawPlot f size bound fileName = do
    image <- newImage size 
    mapM (\p -> let coords = pixelToCoord p size bound in setPixel p (f coords) image) (getPixels size)  
    savePngFile fileName image

-- |Draw points from resolution 
getPixels :: Size -- ^ Resolution of generating image
            -> [Point] -- ^ List of all points
getPixels (width, height) = [(x,y) | x <- [0..(width - 1)], y <- [0..(height - 1)]]

-- |Scaling point to function's coordinates
pixelToCoord :: Point -- ^ Point from resolution
                -> Size -- ^ Resolution of generating image
                -> Bound -- ^ Four coordinates of funtion bound
                -> Coord -- ^Correct coordinates
pixelToCoord (x,y) (xsize, ysize) ((x1,y1), (x2,y2)) =
    ((((x2 - x1) * xscale) + x1), (((y1 - y2) * yscale) + y2))
        where xscale = fromIntegral x / fromIntegral xsize
              yscale = fromIntegral y / fromIntegral ysize
