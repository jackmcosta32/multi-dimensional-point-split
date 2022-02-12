module Connections (connectPoints) where

import System.IO
import Points (Point)
import Lists (popIndex, findIndex) 
import Distances (euclideanDistance)


----------------------------------------------------------------------------------------------------
-- Connect Points
-- Sorts a list of points on an ascending order based on the euclidean distance
-- between each point.
----------------------------------------------------------------------------------------------------
_getNextPointIndex :: Point -> [Float] -> Int
_getNextPointIndex currentPt weights = findIndex (minimum weights) weights

_getNextWeights :: Point -> [Point] -> [Float]
_getNextWeights currentPt pts = [euclideanDistance x y | x <- [currentPt], y <- pts]

_connectPoints :: [Point] -> [Point] -> [Point]
_connectPoints connections pts = do
  let currentPt = last connections
  let weights = _getNextWeights currentPt pts
  let nextIndex = _getNextPointIndex currentPt weights
  let nextPt = pts !! nextIndex
  let nextPts = popIndex nextIndex pts
  let newConnections = connections ++ [nextPt]

  if null nextPts then
    newConnections
  else
    _connectPoints newConnections nextPts

connectPoints :: [Point] -> [Point]
connectPoints [] = []
connectPoints (firstPt : pts) = _connectPoints [firstPt] pts
