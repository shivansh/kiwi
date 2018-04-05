module BPlusTree where

import Helper

-- https://stackoverflow.com/a/12064372/5107319
newtype Tree a = Tree
    { root :: Node a
    } deriving (Show)

data Node a
    = Nil
    | Node { keyCount :: Int -- number of keys currently stored
           , degree :: Int -- degree of a node
           , isLeaf :: Bool -- indicates if the node is leaf
           , keys :: [a]
           , child :: [Node a]
           , next :: Node a -- pointer to next leaf node (if current node is a leaf)
            }
    deriving (Show)

-- create creates a new Tree of a given degree.
-- TODO: Support polymorphic types.
create :: (Num a) => Int -> Tree a
create d =
    Tree
    { root =
          Node
          { keyCount = 0
          , degree = d
          , isLeaf = True
          , keys = []
          , child = []
          , next = Nil
          }
    }

-- search returns the index of the key if it exists in BPlusTree and -1 otherwise.
search :: (Eq a) => Node a -> a -> Int
search x k
    | i <= keyCount x && k == keys x !! i = 1
    | isLeaf x = -1
    | otherwise = search (child x !! i) k
  where
    i = Helper.findKey k (keys x)

-- splitChild splits the i'th child of a Node into two segments, and the middle
-- element of the child is inserted at the i'th index in the Node. The
-- partitioning is done such that the left and the right segments (y and z
-- respectively) can contain atmost (degree - 1) elements.
splitChild :: Node a -> Int -> Node a
splitChild x i =
    let c = child x !! i
        k = take i (keys x) ++ (keys c !! (degree x - 1)) : drop i (keys x)
        z =
            Node
            { keyCount = degree x - 1
            , degree = degree x
            , isLeaf = isLeaf c
            , keys = drop (degree x) (keys c)
            , child = drop (degree x) (child c)
            , next = next c
            }
        y =
            Node
            { keyCount = degree x
            , degree = degree x
            , isLeaf = isLeaf c
            , keys = take (degree x) (keys c) -- left node includes the middle key
            , child = take (degree x) (child c)
            , next = z
            }
    in Node
       { keyCount = keyCount x + 1
       , degree = degree x
       , isLeaf = isLeaf x
       , keys = k
       , child =
             take i (child x) ++
             y : z : drop (Helper.calcIndex i (isLeaf c)) (child x)
       , next = Nil -- parent is no longer a leaf (if it was before)
       }

-- insert inserts a key into the BPlusTree.
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
                     , next = Nil
                     }
             in Tree {root = insertNonFull (splitChild s 0) k}
        else Tree {root = insertNonFull x k}

-- insertNonFull inserts an element into a node having less than 2*(degree) - 1
-- elements.
-- TODO: When inserting to a leaf, the `next` value of the previous leaf needs to be
-- updated.
insertNonFull :: (Ord a) => Node a -> a -> Node a
insertNonFull x k = do
    let i = Helper.insertIndex k (keys x) (keyCount x - 1)
    if isLeaf x
        then Node
             { keyCount = keyCount x + 1
             , degree = degree x
             , isLeaf = isLeaf x
             , keys = take i (keys x) ++ k : drop i (keys x)
             , child = child x
             , next = next x
             }
        else if keyCount (child x !! i) == 2 * degree x - 1
                 -- Luckily, calcIndex behaves exactly as required here.
                 then let x1 = splitChild x i
                          i1 = Helper.calcIndex i (k > (keys x1 !! i))
                      in Node
                         { keyCount = keyCount x1
                         , degree = degree x1
                         , isLeaf = isLeaf x1
                         , keys = keys x1
                         , child =
                               take i1 (child x1) ++
                               insertNonFull (child x1 !! i1) k :
                               drop (i1 + 1) (child x1)
                         , next = next x1
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
                      , next = next x
                      }

-- traverseLeaves linearly traverses all the leaves starting from left.
traverseLeaves :: Show a => Node a -> IO ()
traverseLeaves Nil = putStr ""
traverseLeaves x
    | not (isLeaf x) = traverseLeaves $ head (child x)
    | otherwise = do
        print $ keys x
        traverseLeaves $ next x
