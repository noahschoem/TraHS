module Main where

import System.Environment
import System.Directory
import Control.Monad

import Trahs


main :: IO ()
main = do
  -- performs a one-way sync from dir1 to dir2
  -- read contents of both directories
  [dir1,dir2] <- getArgs  
-- read the corresponding .trahs.db files.
-- If one doesn't exist, initialize it.
  (tradb1,files1) <- readTraDB dir1
  (tradb2,files2) <- readTraDB dir2
  trahs (read tradb1) (files1,dir1) (read tradb2) (files2,dir2)
    

-- Now perform trahs.
-- readTraDB :: String -> IO (String,String)
readTraDB dir = do
  dirContents <- getDirectoryContents dir
  unless (elem ".trahs.db" dirContents) (createNewTRADB dir dirContents)
  tradb <- readFile (dir ++ ".trahs.db")
  let dirContents' = removeJunk dirContents
  return (tradb,dirContents')
  
-- | createNewTRADB: instantiates a .trahs.db file at a given directory 
-- if none exists yet.
-- TODO: finish writing this
createNewTRADB dir dirContents = do
  uid <- genUID
  let fileList = removeJunk dirContents
  let fileMap = 
  let newdb = TraDB {lvn = 1,
                     replicaID = uid,
                     fileData = []} -- this needs to change
  writeFile (dir ++ ".trahs.db") $ show newdb
       
removeJunk :: [FilePath] -> [FilePath]
removeJunk = filter (\x -> x /= "." && x /= ".." && x /= ".trahs.db")

-- needs to change
genUID :: IO String
genUID = return ""

