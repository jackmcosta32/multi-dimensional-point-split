module Distances (euclideanDistance) where

import Points (Point)


----------------------------------------------------------------------------------------------------
-- Euclidean Distance
-- Obtains the euclidean distance between two points.
----------------------------------------------------------------------------------------------------
euclideanDistance :: Point -> Point -> Float
euclideanDistance pt1 pt2 = sqrt (sum [(x - y) ** 2 | (x, y) <- zip pt1 pt2])
