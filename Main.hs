module Main where

import System.Environment
import System.Directory
import Control.Monad
import System.Random

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
  trahs (tradb1 {lvn = lvn tradb1 + 1}) (files1,dir1) (tradb2 {lvn = lvn tradb2 + 1}) (files2,dir2)
    

-- Now perform trahs.
readTraDB :: String -> IO (TraDB,[String])
readTraDB dir = do
  dirContents <- getDirectoryContents dir
  unless (elem ".trahs.db" dirContents) (createNewTRADB dir dirContents)
  tradb <- readFile (dir ++ "/.trahs.db")
  let dirContents' = removeJunk dirContents
  return (read tradb,dirContents')
  
-- | createNewTRADB: instantiates a .trahs.db file at a given directory 
-- if none exists yet.
-- TODO: finish writing this
createNewTRADB dir dirContents = do
  uid <- genUID dir
  writeFile (dir ++ ".trahs.db") $ show $ initDB uid (removeJunk dirContents)

-- this is just so we don't try to sync ., .., or .trahs.db.
removeJunk :: [FilePath] -> [FilePath]
removeJunk = filter (\x -> x /= "." && x /= ".." && x /= ".trahs.db")

-- right now, uids are gotten from the standard random number generator.
genUID :: String -> IO String
genUID = do
  g <- getStdGen
  return $ show $ fst $ next g
