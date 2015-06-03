module Trahs (trahs, initDB) where

import System.Environment
import System.Directory
import Control.Monad
import qualified Codec.Digest.SHA as SHA
import Data.Hashable
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)

-- | trahs: performs a unidirectional sync from dir1 to dir2.
--   Gets passed in arguments for the database info
--   and file list for dir1 and dir2.
trahs :: TraDB -> ([String],String) -> TraDB -> ([String],String) -> IO ()
trahs db1 (files1,dir1) db2 (files2,dir2) = forM_ files1 $ \file -> do
  -- stage 1: checks for any modifications to files in directories 1 and 2
  newdb1 <- checkForMods db1 files1 dir1
  newdb2 <- checkForMods db2 files2 dir2
  -- stage 2: now, sync directories 1 and 2
  
  -- finally, write the databases to the target folders
  
  return ()
  where
    checkForMods db files dir = do
    -- for each file in files, 
    -- compute its hash.
      filesContents <- mapM readFile $ map (dir ++) files
      -- build a map from files in dir to their hashes
      let fileHashes = zip files $ map (SHA.hash SHA.SHA256 . pack) filesContents
    -- if file is not in listed in some file data, 
    -- then add it, and its present hash.
    -- Otherwise, compare the current hash and the old hash.
    -- If they're the same, leave as is.
      let newDatas = M.fromList $ map checkForMod fileHashes
      return $ db {fileData = M.union newDatas oldFileData}
      where
        oldFileData = fileData db
        checkForMod (file,fileHash) = case (M.lookup file oldFileData) of
          Nothing -> (file, FileData {lastHash = fileHash, 
                              versionVector = M.singleton file (lvn db)})
          Just (FileData h vv) -> case (h == fileHash) of
               True -> (file,FileData h vv)
               False-> (file,FileData {lastHash = fileHash,
                                 versionVector = M.insert file (lvn db) vv})
    
    -- Otherwise, update that file's version vector and last hash.
      

{- | FileData: represents the relevant file info for each file
     filename: the name of the file
     lastHash: the SHA256 hash of the last version
     versionVector: a version vector for the current file
-}
data FileData = FileData {lastHash :: BS.ByteString,
                          versionVector :: M.Map String Integer}
  deriving(Eq,Read,Show)

{- | TraDB: represents a database for tra.
     lvn: local version number
     replicaID: the unique identifier of the local directory
     fileData: a list of FileData, one for each file
-}
data TraDB = TraDB {lvn :: Integer, 
                    replicaID :: String, 
                    fileData :: M.Map String FileData}
  deriving(Eq,Read,Show)
  
initDB :: String -> [String] -> TraDB
initDB newID files = TraDB {lvn = 1,
                replicaID = newID,
                fileData = M.empty}-- the fileData most certainly needs to change. 
                