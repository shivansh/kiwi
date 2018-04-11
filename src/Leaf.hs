module Leaf where

import Types

-- getLeafIndex returns the index of the leaf corresponding to the given key.
getLeafIndex :: Int -> Int -> Int
getLeafIndex k m = getLeafIndex' k m 0
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
