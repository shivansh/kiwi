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
create :: Int -> Tree a
create d = do
    let x =
            Node
            {keyCount = 0, degree = d, isLeaf = True, keys = [], child = []}
    let ret = Tree {root = x}
    ret

-- findKey returns the index of key in a list
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
-- partitioning is done such that the left and the right segments can contain
-- atmost t-1 elements.
splitChild :: Node a -> Int -> Node a
splitChild x i = do
    let y = child x !! i
    let x_k = take i (keys x) ++ (keys y !! (degree x - 1)) : drop i (keys x)
    let y1 =
            Node
            { keyCount = degree x - 1
            , degree = degree x
            , isLeaf = isLeaf y
            , keys = take (degree x - 1) (keys y) -- TODO: check
            , child = take (degree x) (child y)
            }
    if not (isLeaf y)
        then do
            let z =
                    Node
                    { keyCount = degree x - 1
                    , degree = degree x
                    , isLeaf = isLeaf y
                    , keys = drop (degree x) (keys y)
                    , child = drop (degree x) (child y)
                    }
            let c1 = take i (child x) ++ y1 : z : drop i (child x)
            let retNode =
                    Node
                    { keyCount = keyCount x + 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = x_k
                    , child = c1
                    }
            retNode
        else do
            let z =
                    Node
                    { keyCount = degree x - 1
                    , degree = degree x
                    , isLeaf = isLeaf y
                    , keys = drop (degree x) (keys y)
                    , child = []
                    }
            let c1 = take i (child x) ++ y1 : z : drop (i + 1) (child x)
            let retNode =
                    Node
                    { keyCount = keyCount x + 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = x_k
                    , child = c1
                    }
            retNode

-- insert inserts a key into the BTree.
insert :: (Num a, Ord a) => Tree a -> a -> Tree a
insert t k = do
    let x = root t
    if keyCount x == 2 * degree x - 1
        then do
            let s =
                    Node
                    { keyCount = 0
                    , degree = degree x
                    , isLeaf = False
                    , keys = []
                    , child = [x]
                    }
            let tree = Tree {root = insertNonFull (splitChild s 0) k}
            tree
        else Tree {root = insertNonFull x k}

-- insertNonFull inserts an element into a node having less than 2*t-1 elements.
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
                 then do
                     let x1 = splitChild x i
                     if k > (keys x1 !! i)
                         then Node
                              { keyCount = keyCount x1
                              , degree = degree x1
                              , isLeaf = isLeaf x1
                              , keys = keys x1
                              , child =
                                    take (i + 1) (child x1) ++
                                    insertNonFull (child x1 !! (i + 1)) k :
                                    drop (i + 2) (child x1)
                              }
                         else Node
                              { keyCount = keyCount x1
                              , degree = degree x1
                              , isLeaf = isLeaf x1
                              , keys = keys x1
                              , child =
                                    take i (child x1) ++
                                    insertNonFull (child x1 !! i) k :
                                    drop (i + 1) (child x1)
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

insertIndex :: (Ord a) => a -> [a] -> Int -> Int
insertIndex x xs i
    | i >= 0 && x < xs !! i = insertIndex x xs (i - 1)
    | otherwise = i + 1
