module Main where

import System.IO
import Helpers (str2Int)
import Points (str2Point)
import Connections (connectPoints)
import Files (requestFile, saveGroupPointIds2File)
import Groups (arrangeConnections, prettyPrintGroups, matchGroupPointIds)


main :: IO ()
main = do
  putStrLn "Primeiro Trabalho Computacional - João Costa"

  file <- requestFile 
  let pts = map str2Point (lines file)
  let connections = connectPoints pts

  putStrLn "Forneca o nome do arquivo de saida: "
  outputFilePath <- getLine

  putStrLn "Forneca o número de grupos (K): "
  strAmount <- getLine  
  let amount = str2Int strAmount

  putStrLn "Agrupamentos:"
  let groups = arrangeConnections amount connections
  let identifiedGroups = matchGroupPointIds groups pts
  prettyPrintGroups identifiedGroups
  saveGroupPointIds2File outputFilePath identifiedGroups

  return ()
