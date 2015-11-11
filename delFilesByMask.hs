import Data.List
import System.Directory

delFile :: String -> IO ()
delFile file = do
  removeFile file
  putStrLn $ "Removing file: " ++ file

delFiles :: String -> IO ()
delFiles mask = do
  files <- getDirectoryContents "."
  mapM_ delFile (filter (isInfixOf mask) files)

main' :: IO ()
main' = do
  putStr "Substring: "
  name <- getLine
  if name == "" then putStrLn "Canceled"
    else delFiles name

main :: IO ()
main = main'