module Disk where

import qualified Data.ByteString.Char8 as C
import Types

dbPath :: DBFile
dbPath = "data/"

-- syncNode saves the contents of a node to disk.
syncNode :: DBFile -> Node a -> IO ()
syncNode leafName = C.writeFile (dbPath ++ leafName) . C.pack . show

-- readNode reads the contents of a node from disk into memory.
readNode :: DBFile -> IO (Node a)
readNode leafName = do
    fileContents <- C.readFile (dbPath ++ leafName)
    let leaf = read (C.unpack fileContents)
    return leaf
