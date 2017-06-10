module Plot
    ( Coordinate
    , drawPlot
    )
    where
import Graphics.GD

type Coordinate = (Double, Double)

drawPlot :: (Coordinate -> Color) -> Size -> String -> IO()
drawPlot f size filename = putStrLn "Draw plot"
