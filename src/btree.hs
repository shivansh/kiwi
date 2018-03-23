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
    let k1 = drop (degree x) (keys y)
    let k2 = take i (keys x) ++ keys y !! degree x : drop i (keys x)
    let y1 =
            Node
            { keyCount = degree x - 1
            , degree = degree x
            , isLeaf = isLeaf y
            , keys = take (degree x - 2) (keys y)
            , child = take (degree x - 1) (child y)
            }
    if not (isLeaf y)
        then do
            let c1 = drop (degree x) (child y)
            let z =
                    Node
                    { keyCount = degree x - 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = k1
                    , child = c1
                    }
            let c2 = take (i - 1) (child x) ++ y1 : z : drop i (child x)
            let retNode =
                    Node
                    { keyCount = keyCount x + 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = k2
                    , child = c2
                    }
            retNode
        else do
            let z =
                    Node
                    { keyCount = degree x - 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = k1
                    , child = []
                    }
            let c1 = take (i - 1) (child x) ++ y1 : z : drop i (child x)
            let retNode =
                    Node
                    { keyCount = keyCount x + 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = k2
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
                    , child = []
                    }
            let tree = Tree {root = insertNonFull (splitChild s 0) 0}
            tree
        else do
            let tree = Tree {root = insertNonFull x k}
            tree

-- isLeafList checks whether the given list contains leaf nodes.
isLeafList :: [Node a] -> Bool
isLeafList x
    | not (null x) = isLeaf $ head x
    | otherwise = True

-- insertNonFull inserts an element into a node having less than 2*t-1 elements.
insertNonFull :: (Ord a) => Node a -> a -> Node a
insertNonFull x k = do
    let i = keyCount x - 1
    if isLeaf x
        then do
            let k1 = take i (keys x) ++ k : drop i (keys x)
            let ret =
                    Node
                    { keyCount = keyCount x + 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = k1
                    , child = child x
                    }
            ret
        else do
            let i1 = insertIndex k (keys x) i
            if keyCount (child x !! i1) == 2 * degree x - 1
                then do
                    let ret = splitChild x i1
                    if k > (keys x !! i1)
                        then insertNonFull (child x !! (i1 + 1)) k
                        else insertNonFull (child x !! i1) k
                else insertNonFull (child x !! i1) k

insertIndex :: (Ord a) => a -> [a] -> Int -> Int
insertIndex x xs i
    | i >= 0 && x < xs !! i = insertIndex x xs (i - 1)
    | otherwise = i + 1
