module Leaf where

import qualified Data.ByteString.Char8 as C
import Disk
import Types

-- getLeafIndex returns the index of the leaf corresponding to the given key.
-- The ith leaf can have a range of keys given by
--      [m*i, m*i + (m-1)], where m is the degree of the B+ Tree.
getLeafIndex :: Int -> Int -> Int
getLeafIndex k1 m1 = getLeafIndex' k1 m1 0
 where
  getLeafIndex' k m i
    | k >= m * i && k <= (m * i) + m - 1 = i
    | k > m * i && k > (m * i) + m - 1   = getLeafIndex' k m (i + 1)
    | otherwise                          = -1

-- getLeafFile returns the filename of the leaf corresponding to the given key.
getLeafFile :: Node a -> IO DBFile
getLeafFile x | null (keys x) = return "l0"
              | -- TODO: assuming sequential insertions
                t == (-1)     = return ""
              | -- TODO: Handle errors
                otherwise     = return ("l" ++ show t)
  where t = getLeafIndex (head (keys x)) (degree x)

-- genLeafName generates a new filename for a leaf depending on the currently
-- allocated filenames available in B+ Tree metadata.
genLeafName :: IO String
genLeafName = do
  metaData <- Disk.readMetaData
  let leafName    = "l" ++ show (1 + leafCount metaData)
  let newMetaData = MetaData $ 1 + leafCount metaData
  syncMetaData newMetaData
  return leafName
