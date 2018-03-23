module Helper where

import Data.List
import Data.Maybe

-- findKey returns the index of a key if found in a list, and -1 otherwise.
findKey :: (Eq a) => a -> [a] -> Int
findKey x xs = fromMaybe (-1) (elemIndex x xs)

-- calcIndex determines the number of elements to be dropped when the creating
-- right segment, depending on whether the node being splitted is a leaf or not.
-- This property is also exploited when inserting in a non-full node.
calcIndex :: Int -> Bool -> Int
calcIndex i isleaf
    | not isleaf = i
    | otherwise = i + 1

-- insertIndex returns the index where a key is to be inserted in a sorted list.
insertIndex :: (Ord a) => a -> [a] -> Int -> Int
insertIndex x xs i
    | i >= 0 && x < xs !! i = insertIndex x xs (i - 1)
    | otherwise = i + 1
