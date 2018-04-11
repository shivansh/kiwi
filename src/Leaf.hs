module Leaf where

import qualified Data.ByteString.Char8 as C
import Disk
import Types

-- getLeafIndex returns the index of the leaf corresponding to the given key.
getLeafIndex :: Int -> Int -> Int
getLeafIndex k1 m1 = getLeafIndex' k1 m1 0
  where
    getLeafIndex' k m i
        | k >= m * i && k <= m * i + m - 1 = i
        | k >= m * i || k >= m * i + m - 1 = getLeafIndex' k m (i + 1)
        | otherwise = -1

-- getLeafFile returns the filename of the leaf corresponding to the given key.
getLeafFile :: Int -> Int -> Maybe DBFile
getLeafFile k m
    | t == -1 = Nothing
    | otherwise = Just $ "l" ++ show t
  where
    t = getLeafIndex k m

-- genLeafName generates a new filename for a leaf depending on the currently
-- allocated filenames available in B+ Tree metadata.
genLeafName :: IO String
genLeafName = do
    metaData <- Disk.readMetaData
    let leafName = "l" ++ show (1 + leafCount metaData)
    let newMetaData = MetaData $ leafCount metaData
    C.writeFile metaFile . C.pack . show $ newMetaData
    return leafName
