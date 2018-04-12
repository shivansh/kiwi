module Disk where

import qualified Data.ByteString.Char8 as C
import Types

dbPath :: DBFile
dbPath = "data/"

metaFile :: DBFile
metaFile = "data/metadata"

-- syncNode saves the contents of a node to disk.
syncNode :: (Show a) => DBFile -> Node a -> IO ()
syncNode leafName = C.writeFile (dbPath ++ leafName) . C.pack . show

-- readNode reads the contents of a node from disk into memory.
readNode :: (Read a) => DBFile -> IO (Node a)
readNode leafName = do
    fileContent <- C.readFile $ dbPath ++ leafName
    let leaf = read $ C.unpack fileContent
    return leaf

-- syncMetaData updates the B+ Tree metadata on disk.
syncMetaData :: MetaData -> IO ()
syncMetaData = C.writeFile metaFile . C.pack . show

-- readMetaData reads the B+ Tree metadata from disk.
readMetaData :: IO MetaData
readMetaData = do
    fileContent <- C.readFile metaFile
    let metadata = read $ C.unpack fileContent
    return metadata
