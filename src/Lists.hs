module Lists (
  qSort,
  popIndex,
  findIndex,
  popIndexes,
  findIndexes
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
-- Pop Index
-- Removes an element from a list given its index.
----------------------------------------------------------------------------------------------------
_popIndexes :: [Int] -> [a] -> Int -> [a] -> [a]
_popIndexes [] xs _ elements = elements ++ xs
_popIndexes _ [] _ _ = []
_popIndexes indexes (x:xs) i elements =
  if (head indexes - i) == 0 then
    _popIndexes (tail indexes) xs (i + 1) elements
  else
    _popIndexes indexes xs (i + 1) (elements ++ [x])

popIndexes :: [Int] -> [a] -> [a]
popIndexes [] a = a
popIndexes _ [] = []
popIndexes indexes xs = do
  let sortedIndexes = qSort indexes
  _popIndexes sortedIndexes xs 0 []

----------------------------------------------------------------------------------------------------
-- Find Index
-- Returns the index of the first match of an element in a list where it might be included.
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
-- Find Indexes
-- Returns a list of indexes where each element of this lists represents the index of a match.
----------------------------------------------------------------------------------------------------
_findIndexes :: Float -> [Float] -> Int -> [Int] -> [Int]
_findIndexes _ [] _ indexes = indexes
_findIndexes x (xx:xxs) i indexes =
  if x == xx then
    _findIndexes x xxs (i + 1) (indexes ++ [i])
  else
    _findIndexes x xxs (i + 1) indexes
    
findIndexes :: Float -> [Float] -> [Int]
findIndexes _ [] = []
findIndexes x xs = _findIndexes x xs 0 []

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
