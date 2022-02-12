module Files where

import System.IO


----------------------------------------------------------------------------------------------------
-- Request File
-- Returns the content of a given file based on its path.
----------------------------------------------------------------------------------------------------
requestFile :: IO String
requestFile = do
  putStrLn "Forneca o nome do arquivo de entrada: "
  inputName <- getLine

  readFile inputName
