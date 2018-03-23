module BTree where

import Data.List
import Data.Maybe

-- https://stackoverflow.com/a/12064372/5107319
newtype Tree a = Tree
    { root :: Node a
    } deriving (Show)

data Node a = Node
    { keyCount :: Int -- number of keys currently stored
    , degree :: Int -- degree of a node
    , isLeaf :: Bool -- indicates if the node is leaf
    , keys :: [a]
    , child :: [Node a]
    } deriving (Show)

-- create creates a new Tree of a given degree.
-- TODO: Support polymorphic types.
create :: (Num a) => Int -> Tree a
create d =
    Tree
    { root =
          Node {keyCount = 0, degree = d, isLeaf = True, keys = [], child = []}
    }

findKey :: (Eq a) => a -> [a] -> Int
findKey x xs = fromMaybe (-1) (elemIndex x xs)

-- search returns the index of the key if it exists in BTree and -1 otherwise.
search :: (Eq a) => Node a -> a -> Int
search x k
    | i <= keyCount x && k == keys x !! i = 1
    | isLeaf x = -1
    | otherwise = search (child x !! i) k
  where
    i = findKey k (keys x)

-- splitChild splits the i'th child of a Node into two segments, and the middle
-- element of the child is inserted at the i'th index in the Node. The
-- partitioning is done such that the left and the right segments (y and z
-- respectively) can contain atmost (degree - 1) elements.
splitChild :: Node a -> Int -> Node a
splitChild x i =
    let c = child x !! i
        k1 = take i (keys x) ++ (keys c !! (degree x - 1)) : drop i (keys x)
        y =
            Node
            { keyCount = degree x - 1
            , degree = degree x
            , isLeaf = isLeaf c
            , keys = take (degree x - 1) (keys c)
            , child = take (degree x) (child c)
            }
        z =
            Node
            { keyCount = degree x - 1
            , degree = degree x
            , isLeaf = isLeaf c
            , keys = drop (degree x) (keys c)
            , child = drop (degree x) (child c)
            }
    in Node
       { keyCount = keyCount x + 1
       , degree = degree x
       , isLeaf = isLeaf x
       , keys = k1
       , child =
             take i (child x) ++ y : z : drop (calcIndex i (isLeaf c)) (child x)
       }

-- insert inserts a key into the BTree.
insert :: (Ord a) => Tree a -> a -> Tree a
insert t k = do
    let x = root t
    if keyCount x == 2 * degree x - 1
        then let s =
                     Node
                     { keyCount = 0
                     , degree = degree x
                     , isLeaf = False
                     , keys = []
                     , child = [x]
                     }
             in Tree {root = insertNonFull (splitChild s 0) k}
        else Tree {root = insertNonFull x k}

-- insertNonFull inserts an element into a node having less than 2*(degree) - 1
-- elements.
insertNonFull :: (Ord a) => Node a -> a -> Node a
insertNonFull x k = do
    let i = insertIndex k (keys x) (keyCount x - 1)
    if isLeaf x
        then Node
             { keyCount = keyCount x + 1
             , degree = degree x
             , isLeaf = isLeaf x
             , keys = take i (keys x) ++ k : drop i (keys x)
             , child = child x
             }
        else if keyCount (child x !! i) == 2 * degree x - 1
                 -- Luckily, calcIndex behaves exactly as required here.
                 then let x1 = splitChild x i
                          i1 = calcIndex i (k > (keys x1 !! i))
                      in Node
                         { keyCount = keyCount x1
                         , degree = degree x1
                         , isLeaf = isLeaf x1
                         , keys = keys x1
                         , child =
                               take i1 (child x1) ++
                               insertNonFull (child x1 !! i1) k :
                               drop (i1 + 1) (child x1)
                         }
                 else Node
                      { keyCount = keyCount x
                      , degree = degree x
                      , isLeaf = isLeaf x
                      , keys = keys x
                      , child =
                            take i (child x) ++
                            insertNonFull (child x !! i) k :
                            drop (i + 1) (child x)
                      }

-- calcIndex determines the number of elements to be dropped when the creating
-- right segment, depending on whether the node being splitted is a leaf or not.
-- This property is also exploited when inserting in a non-full node.
calcIndex :: Int -> Bool -> Int
calcIndex i isleaf
    | not isleaf = i
    | otherwise = i + 1

insertIndex :: (Ord a) => a -> [a] -> Int -> Int
insertIndex x xs i
    | i >= 0 && x < xs !! i = insertIndex x xs (i - 1)
    | otherwise = i + 1
