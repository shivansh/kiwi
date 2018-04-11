module BPlusTree where

import qualified Data.ByteString.Char8 as C
import Disk
import Helper
import Leaf
import Types

-- create creates a new Tree of a given degree.
-- TODO: Support polymorphic types.
create :: (Num a) => Int -> IO (Tree a)
create d = do
    let newMetaData = MetaData 0
    C.writeFile metaFile . C.pack . show $ newMetaData
    return
        Tree
        { root =
              Node
              { keyCount = 0
              , degree = d
              , isLeaf = True
              , keys = []
              , child = []
              , next = Nothing
              }
        }

-- search returns the index of the key if it exists in BPlusTree and -1 otherwise.
search :: (Eq a) => Node a -> Int -> Int
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
splitChild :: (Ord a) => Node a -> Int -> IO (Node a)
splitChild x i = do
    let c = child x !! i
    let k = take i (keys x) ++ (keys c !! degree x) : drop i (keys x)
    let z =
            Node
            { keyCount = degree x - 2
            , degree = degree x
            , isLeaf = isLeaf c
            , keys = drop (degree x + 1) (keys c)
            , child = drop (degree x + 1) (child c)
            , next = next c
            }
    rightLeaf <- genLeafName
    let y =
            Node
            { keyCount = degree x + 1
            , degree = degree x
            , isLeaf = isLeaf c
            , keys = take (degree x + 1) (keys c)
            , child = take (degree x + 1) (child c)
            , next = Just rightLeaf
            }
    let ret =
            Node
            { keyCount = keyCount x + 1
            , degree = degree x
            , isLeaf = isLeaf x
            , keys = k
            , child =
                  take i (child x) ++
                  y : z : drop (Helper.calcIndex i (isLeaf c)) (child x)
            , next = Nothing -- parent is no longer a leaf (if it was before)
            }
    if isLeaf c
        -- Sync updates to leaves on disk.
        then do
            leftLeaf <- getLeafFile y
            Disk.syncNode leftLeaf y
            Disk.syncNode rightLeaf z
            metaData <- Disk.readMetaData
            let newMetaData = MetaData $ 1 + leafCount metaData
            C.writeFile metaFile . C.pack . show $ newMetaData
            return ret
        else return ret

-- insert inserts a key into the BPlusTree.
insert :: (Ord a) => Tree a -> Int -> IO (Tree a)
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
                    , next = Nothing
                    }
            updatedChild <- splitChild s 0
            updatedRoot <- insertNonFull updatedChild k
            return Tree {root = updatedRoot}
        else do
            updatedRoot <- insertNonFull x k
            return Tree {root = updatedRoot}

-- insertNonFull inserts an element into a node having less than 2*(degree) - 1
-- elements.
insertNonFull :: (Ord a) => Node a -> Int -> IO (Node a)
insertNonFull x k = do
    let i = Helper.insertIndex k (keys x) (keyCount x - 1)
    if isLeaf x
        then do
            let ret =
                    Node
                    { keyCount = keyCount x + 1
                    , degree = degree x
                    , isLeaf = isLeaf x
                    , keys = take i (keys x) ++ k : drop i (keys x)
                    , child = child x
                    , next = next x
                    }
            leafName <- getLeafFile x
            syncNode leafName ret
            return ret
        else if keyCount (child x !! i) == 2 * degree x - 1
                 then do
                     x1 <- splitChild x i
                     let i1 = Helper.calcIndex i (k > (keys x1 !! i))
                     updatedChild <- insertNonFull (child x1 !! i1) k
                     return
                         Node
                         { keyCount = keyCount x1
                         , degree = degree x1
                         , isLeaf = isLeaf x1
                         , keys = keys x1
                         , child =
                               take i1 (child x1) ++
                               updatedChild : drop (i1 + 1) (child x1)
                         , next = next x1
                         }
                 else do
                     updatedChild <- insertNonFull (child x !! i) k
                     return
                         Node
                         { keyCount = keyCount x
                         , degree = degree x
                         , isLeaf = isLeaf x
                         , keys = keys x
                         , child =
                               take i (child x) ++
                               updatedChild : drop (i + 1) (child x)
                         , next = next x
                         }
