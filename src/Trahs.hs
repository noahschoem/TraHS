module Trahs (trahs) where

import Codec.Digest.SHA
import qualified Data.Map as M
import Data.ByteString

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
  
  where
    checkForUpdates 

{- | FileData: represents the relevant file info for each file
     filename: the name of the file
     lastHash: the SHA256 hash of the last version
     versionVector: a version vector for the current file
-}
data FileData = FileData {filename :: String,
                          lastHash :: ByteString,
                          versionVector :: M.Map String Integer}
  deriving(Eq,Read,Show)

{- | TraDB: represents a database for tra.
     lvn: local version number
     replicaID: the unique identifier of the local directory
     fileData: a list of FileData, one for each file
-}
data TraDB = TraDB {lvn :: Integer, 
                    replicaID :: String, 
                    fileData :: [FileData]}
  deriving(Eq,Read,Show)