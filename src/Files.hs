module Files where

import System.IO
import Groups (Group)

----------------------------------------------------------------------------------------------------
-- Request File
-- Returns the content of a given file based on its path.
----------------------------------------------------------------------------------------------------
requestFile :: IO String
requestFile = do
  putStrLn "Forneca o nome do arquivo de entrada: "
  inputName <- getLine

  readFile inputName

----------------------------------------------------------------------------------------------------
-- Save File
-- Returns the content of a given file based on its path.
----------------------------------------------------------------------------------------------------
-- saveGroupId2File :: String -> [[Int]] -> IO ()
-- saveGroupId2File "" _ = return ()
-- saveGroupId2File _ [] = return ()
-- saveGroupId2File path ids = do
  
_addPointId2File :: Show a => FilePath -> [a] -> IO ()
_addPointId2File "" _ = return ()
_addPointId2File _ [] = return ()
_addPointId2File path (pointId:pointIds) = do
  appendFile path (show pointId)
  
  if null pointIds then
    appendFile path "\n"
  else do
    appendFile path ","
    _addPointId2File path pointIds

saveGroupPointIds2File :: Show a => FilePath -> [[a]] -> IO ()
saveGroupPointIds2File "" _ = return ()
saveGroupPointIds2File _ [] = return ()
saveGroupPointIds2File path (pointIds:groupPointIds) = do
  _addPointId2File path pointIds
  saveGroupPointIds2File path groupPointIds