module Plot
    ( Coord
    , Bound
    , drawPlot
    ) where
import Graphics.GD

type Coord = (Double, Double)
type Bound = (Coord, Coord)

drawPlot :: (Coord -> Color) -> Size -> Bound -> String -> IO()
drawPlot f size bound fileName = do
    image <- newImage size
    mapM (\p -> let coords = pixelToCoord p size bound in setPixel p (f coords) image) (getPixels size)
    savePngFile fileName image

getPixels :: Size -> [Point]
getPixels (width, height) = [(x,y) | x <- [0..(width - 1)], y <- [0..(height - 1)]]

pixelToCoord :: Point -> Size -> Bound -> Coord
pixelToCoord (x,y) (xsize, ysize) ((x1,y1), (x2,y2)) =
    ((((x2 - x1) * xscale) + x1), (((y1 - y2) * yscale) + y2))
        where xscale = fromIntegral x / fromIntegral xsize
              yscale = fromIntegral y / fromIntegral ysize
