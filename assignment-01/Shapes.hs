module Shapes
where

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

data Shape
  =  Circle Double            -- radius
  |  Square Double            -- length
  |  Rectangle Double Double  -- length and width
  deriving (Show)

showShape :: Shape -> String
showShape (Circle r)       =  "circle of radius " ++ show r
showShape (Square l)       =  "square of length " ++ show l
showShape (Rectangle l w)  =  "rectangle of length " ++ show l
                                ++ " and width " ++ show w

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Square l) = l * l
area (Rectangle l w) = l * w

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = 2 * l
perimeter (Rectangle l w) = 2 * (l + w)

center :: Shape -> (Double, Double)  -- x- and y-coordinates
center (Circle r) = (0, 0)
center (Square l) = (l / 2, l / 2)
center (Rectangle l w) = (w / 2, l / 2)

boundingBox :: Shape -> (Double, Double)  -- width and height
boundingBox (Circle r) = (2 * r, 2 * r)
boundingBox (Square l) = (l, l)
boundingBox (Rectangle l w) = (w, l)
