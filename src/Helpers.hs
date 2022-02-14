module Helpers where

import Data.List (elemIndex)
import Control.Monad(when)

minValue :: Ord a => [a] -> a
minValue [] = undefined
minValue [x1] = x1
minValue [x1, x2] = min x1 x2
minValue (x1:x2:xs) = min (min x1 x2) (minValue xs)

minValueIndex :: Ord a => [a] -> Maybe Int
minValueIndex xs = elemIndex (minValue xs) xs

maxValue :: Ord a => [a] -> a
maxValue [] = undefined
maxValue [x1] = x1
maxValue [x1, x2] = max x1 x2
maxValue (x1:x2:xs) = max (max x1 x2) (maxValue xs)

maxValueIndex :: Ord a => [a] -> Maybe Int
maxValueIndex xs = elemIndex (maxValue xs) xs

str2Int :: String -> Int
str2Int string = read string::Int

str2Float :: String -> Float
str2Float string = read string::Float

splitString :: (Char -> Bool) -> String -> [String]
splitString condition string = case dropWhile condition string of
  "" -> []
  string' -> word: splitString condition string''
    where (word, string'') = break condition string'
