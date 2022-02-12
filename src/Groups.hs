module Groups (
  prettyPrintGroups,
  arrangeConnections
) where

import System.IO
import Points (Point)
import Distances (euclideanDistance)
import Lists (popIndex, findIndex, qSort) 

type Group = [Point]


----------------------------------------------------------------------------------------------------
-- Arrange Connections
-- Reorganizes a connection into a list of groups given the amount of groups.
----------------------------------------------------------------------------------------------------
_getBreakpoints :: Int -> [Float] -> [Int] -> [Int]
_getBreakpoints a [] b = b
_getBreakpoints amount distances breakpoints = do
  let maxDistance = maximum distances
  let index = findIndex maxDistance distances
  let newDistances = popIndex index distances
  let newBreakPoints = breakpoints ++ [index + 1]

  if amount == 0 then
    newBreakPoints
  else
    _getBreakpoints (amount - 1) newDistances newBreakPoints

_splitConnections :: [Int] -> Group -> [Group] -> [Group]
_splitConnections [] a _ = [a] 
_splitConnections (bktp:bkpts) connections connectionGroups = do
  let (connectionGroup, newConnections) = splitAt bktp connections
  let newConnectionGroups = connectionGroups ++ [connectionGroup]

  if null bkpts then
    newConnectionGroups
  else
    _splitConnections bkpts newConnections newConnectionGroups

arrangeConnections :: Int -> Group -> [Group]
arrangeConnections 0 _ = []
arrangeConnections 1 a = [a]
arrangeConnections amount connections = do
  let groupedConnections = zip (init connections) (tail connections)
  let distances = [euclideanDistance x y | (x, y) <- groupedConnections]
  let bkpts = _getBreakpoints (amount - 1) distances []
  let sortedBkpts = qSort bkpts

  _splitConnections sortedBkpts connections []

----------------------------------------------------------------------------------------------------
-- Pretty Print Groups 
-- Prints a list of groups separating each group per line.
----------------------------------------------------------------------------------------------------
prettyPrintGroups :: [Group] -> IO ()
prettyPrintGroups [] = return ()
prettyPrintGroups (group:groups) = do
  print group

  if null groups then
    return ()
  else
    prettyPrintGroups groups
