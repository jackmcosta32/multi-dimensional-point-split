module Helpers (
  str2Int,
  str2Float,
  splitString
) where


----------------------------------------------------------------------------------------------------
-- String to Integer
-- Casts a string into an integer.
----------------------------------------------------------------------------------------------------
str2Int :: String -> Int
str2Int string = read string::Int

----------------------------------------------------------------------------------------------------
-- String to Float
-- Casts a string into a float.
----------------------------------------------------------------------------------------------------
str2Float :: String -> Float
str2Float string = read string::Float

----------------------------------------------------------------------------------------------------
-- Split String
-- Splits a string at a certain point that fulfill a certain condition.
----------------------------------------------------------------------------------------------------
splitString :: (Char -> Bool) -> String -> [String]
splitString condition string = case dropWhile condition string of
  "" -> []
  string' -> word: splitString condition string''
    where (word, string'') = break condition string'
