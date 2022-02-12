module Main where

import Group
import Helpers
import System.IO
import Control.Monad(when)
import Data.List(elemIndex)
-- import System.Directory
import Helpers (str2Int, str2Float)
import Distance (distances)

-- Ver método break


-- 1
-- Obtains the content of a file given its path
requestFile :: IO String
requestFile = do
  putStrLn "Forneca o nome do arquivo de entrada: "
  inputName <- getLine

  -- when doesFileExist inputName
  --   putStrLn $ "O arquivo de nome " ++ inputName ++ " não pode ser encontrado."
  --  requestFile

  file <- readFile inputName
  return file


-- 2
-- Converts a point string into a point
castLine2Point :: String -> [Float]
castLine2Point line = map str2Float (splitString (==',') line)

-- Casts the whole file into a point array
castFile2PointArray :: String -> [[Float]]
castFile2PointArray file = map castLine2Point (lines file)


-- 3
euclideanDistance :: [Float] -> [Float] -> Float
euclideanDistance pt1 pt2 = sum [(x - y) ** 2 | (x, y) <- zip pt1 pt2]

popIndex :: Int -> [a] -> [a]
popIndex _ [] = []
popIndex index (x:xs)
  | index == 0 = xs
  | otherwise = x : popIndex (index - 1) xs

_findIndex :: Float -> [Float] -> Int -> Int
_findIndex _ [] _ = -1
_findIndex x (xx:xxs) i
  | x == xx = i
  | otherwise = _findIndex x xxs (i + 1) 

findIndex :: Float -> [Float] -> Int
findIndex _ [] = -1
findIndex x xs = _findIndex x xs 0 

getNextPointIndex :: [Float] -> [Float] -> Int
getNextPointIndex currentPt weights = findIndex (minimum weights) weights

-- Calculates the weight of the connections given a point and a list of possible connections
getNextWeights :: [Float] -> [[Float]] -> [Float]
getNextWeights currentPt pts = [euclideanDistance x y | x <- [currentPt], y <- pts]

_getConnections :: [[Float]] -> [[Float]] -> [[Float]]
_getConnections connections pts = do
  let currentPt = last connections
  let weights = getNextWeights currentPt pts
  let nextIndex = getNextPointIndex currentPt weights
  let nextPt = pts !! nextIndex
  let nextPts = popIndex nextIndex pts
  let newConnections = connections ++ [nextPt]

  if null nextPts then
    newConnections
  else
    _getConnections newConnections nextPts

-- Generates a list of connections given a list of points
getConnections :: [[Float]] -> [[Float]]
getConnections [] = []
getConnections (firstPt : pts) = _getConnections [firstPt] pts

-- _groupConnections :: Int -> [Float] -> [[[Float]]] -> [[[Float]]]
-- _groupConnections amount distances connectionGroups = do
--   let maxDistance = maximum distances
--   let index = findIndex maxDistance distances
--   let newDistances = popIndex index distances
  
--   if amount == 0 then
--     connectionGroups
--   else
--     _groupConnections (amount - 1) newDistances connectionGroups

-- 4
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- Generates a list of index where the connections list should be splitted in order to form the amout of groups specified
getBreakpoints :: Int -> [Float] -> [Int] -> [Int]
getBreakpoints a [] b = b
getBreakpoints amount distances breakpoints = do
  let maxDistance = maximum distances
  let index = findIndex maxDistance distances
  let newDistances = popIndex index distances
  let newBreakPoints = breakpoints ++ [index + 1]

  if amount == 0 then
    newBreakPoints
  else
    getBreakpoints (amount - 1) newDistances newBreakPoints

-- Splits a group of connections into a list of groups given the amount needed and the inital group
splitConnections :: [Int] -> [[Float]] -> [[[Float]]] -> [[[Float]]]
splitConnections [] a _ = [a] 
splitConnections (bktp:bkpts) connections connectionGroups = do
  let (connectionGroup, newConnections) = splitAt bktp connections
  let newConnectionGroups = connectionGroups ++ [connectionGroup]

  if null bkpts then
    newConnectionGroups
  else
    splitConnections bkpts newConnections newConnectionGroups

-- Generates a group of connections given the amount of groups needed and a list of connections 
groupConnections :: Int -> [[Float]] -> [[[Float]]]
groupConnections 0 _ = []
groupConnections 1 a = [a]
groupConnections amount connections = do
  let groupedConnections = zip (init connections) (tail connections)
  let distances = [euclideanDistance x y | (x, y) <- groupedConnections]
  let bkpts = getBreakpoints (amount - 1) distances []
  let sortedBkpts = qsort bkpts

  splitConnections sortedBkpts connections []

-- 5
prettyPrintGroups :: [[[Float]]] -> IO ()
prettyPrintGroups [] = return ()
prettyPrintGroups (group:groups) = do
  print group

  if null groups then
    return ()
  else
    prettyPrintGroups groups

main :: IO ()
main = do
  putStrLn "Primeiro Trabalho Computacional - João Costa"

  file <- requestFile
  let pts = castFile2PointArray file
  let connections = getConnections pts

  putStrLn "Forneca o nome do arquivo de saida: "
  outputName <- getLine

  putStrLn "Forneca o número de grupos (K): "
  strGroupAmout <- getLine  
  let groupAmout = str2Int strGroupAmout

  putStrLn "Agrupamentos:"

  let groups = groupConnections groupAmout connections 
  prettyPrintGroups groups

  return ()
