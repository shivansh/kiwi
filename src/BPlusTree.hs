module BPlusTree where

import qualified Data.ByteString.Char8 as C
import Disk
import Helper
import Leaf
import Types

-- create creates a new Tree of a given degree.
create :: Int -> IO (Tree a)
create d = do
    let newMetaData = MetaData 0
    C.writeFile metaFile . C.pack . show $ newMetaData
    return
        Tree
        { root =
              Leaf
              {keyCount = 0, degree = d, keys = [], values = [], next = Nothing}
        }

-- search returns the index of the key if it exists in BPlusTree and -1 otherwise.
search :: Node a -> Int -> Int
search x k
    | i <= keyCount x && k == keys x !! i = 1
    | otherwise = search (child x !! i) k
  where
    i = Helper.findKey k (keys x)

-- splitChild splits the i'th child of a Node into two segments, and the middle
-- element of the child is inserted at the i'th index in the Node. The
-- partitioning is done such that the left and the right segments (y and z
-- respectively) can contain atmost (degree - 1) elements.
splitChild :: (Show a) => Node a -> Int -> IO (Node a)
splitChild x@Node {} i = do
    let c = child x !! i
    let k = take i (keys x) ++ (keys c !! degree x) : drop i (keys x)
    case c of
        children@Node {} -> do
            let z =
                    Node
                    { keyCount = degree x - 2
                    , degree = degree x
                    , keys = drop (degree x + 1) (keys children)
                    , child = drop (degree x + 1) (child children)
                    }
            let y =
                    Node
                    { keyCount = degree x + 1
                    , degree = degree x
                    , keys = take (degree x + 1) (keys children)
                    , child = take (degree x + 1) (child children)
                    }
            return
                Node
                { keyCount = keyCount x + 1
                , degree = degree x
                , keys = k
                , child =
                      take i (child x) ++
                      y : z : drop (Helper.calcIndex i False) (child x)
                }
        children@Leaf {} -> do
            let z =
                    Leaf
                    { keyCount = degree x - 2
                    , degree = degree x
                    , keys = drop (degree x + 1) (keys children)
                    , values = drop (degree x + 1) (values children)
                    , next = next c
                    }
            rightLeaf <- genLeafName
            let y =
                    Leaf
                    { keyCount = degree x + 1
                    , degree = degree x
                    , keys = take (degree x + 1) (keys children)
                    , values = take (degree x + 1) (values children)
                    , next = Just rightLeaf
                    }
            let ret =
                    Node
                    { keyCount = keyCount x + 1
                    , degree = degree x
                    , keys = k
                    , child =
                          take i (child x) ++
                          y : z : drop (Helper.calcIndex i True) (child x)
                    }
            -- Sync updates to leaves on disk.
            leftLeaf <- getLeafFile y
            Disk.syncNode leftLeaf y
            -- The right leaf (z) doesn't need to be synced to disk as it is
            -- currently empty.
            return ret
        Nil -> error "Cannot split Nil type"
splitChild Leaf {} _ = error "Cannot split Leaf type"
splitChild Nil _ = error "Cannot split Nil type"

-- insert inserts a key into the BPlusTree.
insert :: (Show a) => Tree a -> Int -> a -> IO (Tree a)
insert t k v = do
    let x = root t
    if keyCount x == 2 * degree x - 1
        then do
            let s =
                    Node
                    {keyCount = 0, degree = degree x, keys = [], child = [x]}
            updatedChild <- splitChild s 0
            updatedRoot <- insertNonFull updatedChild k v
            return Tree {root = updatedRoot}
        else do
            updatedRoot <- insertNonFull x k v
            return Tree {root = updatedRoot}

-- insertNonFull inserts an element into a node having less than 2*(degree) - 1
-- elements.
insertNonFull :: (Show a) => Node a -> Int -> a -> IO (Node a)
insertNonFull x@Node {} k v = do
    let i = Helper.insertIndex k (keys x) (keyCount x - 1)
    if keyCount (child x !! i) == 2 * degree x - 1
        then do
            x1 <- splitChild x i
            let i1 = Helper.calcIndex i (k > (keys x1 !! i))
            updatedChild <- insertNonFull (child x1 !! i1) k v
            return
                Node
                { keyCount = keyCount x1
                , degree = degree x1
                , keys = keys x1
                , child =
                      take i1 (child x1) ++
                      updatedChild : drop (i1 + 1) (child x1)
                }
        else do
            updatedChild <- insertNonFull (child x !! i) k v
            return
                Node
                { keyCount = keyCount x
                , degree = degree x
                , keys = keys x
                , child =
                      take i (child x) ++ updatedChild : drop (i + 1) (child x)
                }
insertNonFull x@Leaf {} k v = do
    let i = Helper.insertIndex k (keys x) (keyCount x - 1)
    let ret =
            Leaf
            { keyCount = keyCount x + 1
            , degree = degree x
            , keys = take i (keys x) ++ k : drop i (keys x)
            , values = take i (values x) ++ v : drop i (values x)
            , next = next x
            }
    leafName <- getLeafFile ret
    syncNode leafName ret
    return ret
insertNonFull Nil _ _ = error "Cannot insert in Nil type"
