module Points (
  Point,
  str2Point
) where

import Helpers (str2Float, splitString)

type Point = [Float]


----------------------------------------------------------------------------------------------------
-- Str2Point
-- Converts a string like "1,1", to a point type variable [1, 1].
----------------------------------------------------------------------------------------------------
str2Point :: String -> Point
str2Point line = map str2Float (splitString (==',') line)
