module Main where

import System.Environment
import System.Directory
import Control.Monad

import Trahs


main :: IO ()
main = do
  -- read contents of both directories
  [dir1,dir2] <- getArgs  
-- read the corresponding .trahs.db files.
-- If one doesn't exist, initialize it.
  (tradb1,files1) <- readTraDB dir1
  (tradb2,files2) <- readTraDB dir2
  trahs (read tradb1) files1 (read tradb2) files2
    

-- Now perform trahs.
-- readTraDB :: String -> IO (String,String)
readTraDB dir = do
  dirContents <- getDirectoryContents dir
  unless (elem ".trahs.db" dirContents) (createNewTRADB dir)
  tradb <- readFile (dir ++ ".trahs.db")
  let dirContents' =  filter (\x -> x /= "." && x /= ".." && x /= ".trahs.db") dirContents
  return (tradb,dirContents')
  
-- | createNewTRADB: instantiates a .trahs.db file at a given directory 
-- if none exists yet.
-- TODO: finish writing this
createNewTRADB dir = do
  uid <- genUID
  let fileList = "[]" -- will have to change
  let newdb = "1\n" ++ uid ++ "\n"
  writeFile (dir ++ ".trahs.db") $ show newdb
       
-- needs to change
genUID :: IO String
genUID = return ""

