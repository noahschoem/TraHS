module Trahs (trahs) where

import Codec.Digest.SHA
import qualified Data.Map as M
import Data.ByteString

trahs :: TraDB -> [String] -> TraDB -> [String] -> IO ()
trahs db1 files1 db2 files2 = do
  return ()

data FileData = FileData {lastHash :: ByteString,
                          versionVector :: M.Map String Integer}
  deriving(Eq,Read,Show)

data TraDB = TraDB {lvn :: Integer, 
                    replicaID :: String, 
                    fileData :: [FileData]}
  deriving(Eq,Read,Show)
-- should probably make version vectors as maps