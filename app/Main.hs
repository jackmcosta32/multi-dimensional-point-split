module Main where

import System.IO
import Helpers (str2Int)
import Points (str2Point)
import Files (requestFile)
import Connections (connectPoints)
import Groups (arrangeConnections, prettyPrintGroups)


main :: IO ()
main = do
  putStrLn "Primeiro Trabalho Computacional - João Costa"

  file <- requestFile
  let pts = map str2Point (lines file)
  let connections = connectPoints pts

  putStrLn "Forneca o nome do arquivo de saida: "
  outputName <- getLine

  putStrLn "Forneca o número de grupos (K): "
  strAmount <- getLine  
  let amount = str2Int strAmount

  putStrLn "Agrupamentos:"
  let groups = arrangeConnections amount connections 
  prettyPrintGroups groups

  return ()
