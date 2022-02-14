module Groups (
  Group,
  matchGroupPointIds,
  getBreakpoints,
  prettyPrintGroups,
  arrangeConnections
) where

import System.IO
import Points (Point)
import Distances (euclideanDistance)
import Lists (popIndex, findIndex, qSort, popIndexes, findIndexes)

type Group = [Point]


----------------------------------------------------------------------------------------------------
-- Get Breakpoints
-- Returns the index of were the connection must be splitted in order to form
-- the requested amount of groups.
----------------------------------------------------------------------------------------------------
_getBreakpoints :: Int -> [Float] -> [Float] -> [Int] -> [Int]
_getBreakpoints a _ [] b = b
_getBreakpoints a [] _ b = b
_getBreakpoints amount weights weightHelpers breakpoints = do
  let maxWeight = maximum weightHelpers
  let indexes = take amount (reverse (findIndexes maxWeight weights))
  let helperIndexes = findIndexes maxWeight weightHelpers
  let newWeightHelpers = popIndexes helperIndexes weightHelpers
  let newBreakPoints = breakpoints ++ indexes

  if amount <= 1 then
    newBreakPoints
  else
    _getBreakpoints (amount - length indexes) weights newWeightHelpers newBreakPoints

getBreakpoints :: Int -> [Float] -> [Int]
getBreakpoints amount weights = do
  let bkpts = _getBreakpoints amount weights weights []
  [b + 1 | b <- bkpts]

----------------------------------------------------------------------------------------------------
-- Arrange Connections
-- Reorganizes a connection into a list of groups given the amount of groups.
----------------------------------------------------------------------------------------------------
_splitConnections :: [Int] -> [a] -> [[a]] -> [[a]]
_splitConnections [] a _ = [a]
_splitConnections (bkpt:bkpts) connections connectionGroups = do
  let (connectionGroup, newConnections) = splitAt bkpt connections
  let newConnectionGroups = connectionGroups ++ [connectionGroup]
  let newBkpts = [b - bkpt | b <- bkpts]

  if null bkpts then
    newConnectionGroups ++ [newConnections]
  else
    _splitConnections newBkpts newConnections newConnectionGroups

arrangeConnections :: Int -> Group -> [Group]
arrangeConnections 0 _ = []
arrangeConnections 1 a = [a]
arrangeConnections amount connections = do
  let groupedConnections = zip (init connections) (tail connections)
  let weights = [euclideanDistance x y | (x, y) <- groupedConnections]
  let bkpts = getBreakpoints (amount - 1) weights
  let sortedBkpts = qSort bkpts

  _splitConnections sortedBkpts connections []

----------------------------------------------------------------------------------------------------
-- Match Group Ids 
-- Converts a group array, replacing their points to the point respective id.
----------------------------------------------------------------------------------------------------
-- _connections2Ids :: Eq a => [[a]] -> [a] -> [[Int]] -> [[Int]]
_matchPointIds :: Eq a => [a] -> [a] -> [Int] -> [Int]
_matchPointIds [] _ indexes = indexes
_matchPointIds (currentPoint:group) points indexes = do
  let index = findIndex currentPoint points + 1
  _matchPointIds group points (indexes ++ [index])

_matchGroupPointIds :: [Group] -> [Point] -> [[Int]] -> [[Int]]
_matchGroupPointIds [] _ groupIndexes = groupIndexes
_matchGroupPointIds (group:groups) points groupIndexes = do
  let indexes = _matchPointIds group points []
  _matchGroupPointIds groups points (groupIndexes ++ [indexes])

matchGroupPointIds :: [Group] -> [Point] -> [[Int]]
matchGroupPointIds groups points = _matchGroupPointIds groups points []

----------------------------------------------------------------------------------------------------
-- Pretty Print Groups 
-- Prints a list of groups separating each group per line.
----------------------------------------------------------------------------------------------------
prettyPrintGroups :: Show a => [a] -> IO ()
prettyPrintGroups [] = return ()
prettyPrintGroups (group:groups) = do
  print group

  if null groups then
    return ()
  else
    prettyPrintGroups groups
