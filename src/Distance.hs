module Distance where

type Point = (Float, Float)

euclidean :: Floating a => (a, a) -> (a, a) -> a
euclidean (x1, y1) (x2, y2) = sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)

distances :: (t1 -> t2 -> a) -> t1 -> [t2] -> [a]
distances metric p0 [] = []
distances metric p0 ps = [metric p0 p | p <- ps]
