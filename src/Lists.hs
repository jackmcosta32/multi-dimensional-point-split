module Lists (
  qSort,
  popIndex,
  findIndex
) where


----------------------------------------------------------------------------------------------------
-- Pop Index
-- Removes an element from a list given its index.
----------------------------------------------------------------------------------------------------
popIndex :: Int -> [a] -> [a]
popIndex _ [] = []
popIndex index (x:xs)
  | index == 0 = xs
  | otherwise = x : popIndex (index - 1) xs

----------------------------------------------------------------------------------------------------
-- Find Index
-- Returns the index of an element given the element and the list where it might be included.
----------------------------------------------------------------------------------------------------
_findIndex :: Float -> [Float] -> Int -> Int
_findIndex _ [] _ = -1
_findIndex x (xx:xxs) i
  | x == xx = i
  | otherwise = _findIndex x xxs (i + 1) 

findIndex :: Float -> [Float] -> Int
findIndex _ [] = -1
findIndex x xs = _findIndex x xs 0

----------------------------------------------------------------------------------------------------
-- Quick Sort
-- Sorts a given list on an ascending order.
----------------------------------------------------------------------------------------------------
qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) =
  qSort smaller ++ [x] ++ qSort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
