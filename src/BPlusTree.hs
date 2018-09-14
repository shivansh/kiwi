module BPlusTree where

import qualified Data.ByteString.Char8 as C
import Disk
import Helper
import Leaf
import System.Directory
import Types

-- create creates a new Tree of a given degree.
create :: Int -> IO (Tree a)
create d = do
  let newMetaData = MetaData 0
  createDirectory dbPath
  syncMetaData newMetaData
  return Tree
    { root = Leaf
      { keyCount = 0
      , degree   = d
      , keys     = []
      , values   = []
      , next     = Nothing
      }
    }

-- search returns the index of the key if it exists in BPlusTree and -1
-- otherwise.
search :: Node a -> Int -> a
search x k = case x of
  c@Node{} | i == -1 -> search (child c !! relevantIndex 0 k (keys c)) k
           | i <= keyCount c && k == keys c !! i -> search (child c !! i) k
           | otherwise -> error "Key does not exist"
  c@Leaf{} | i == -1                             -> error "Key does not exist"
           | i <= keyCount c && k == keys c !! i -> values c !! i
           | otherwise                           -> error "Key does not exist"
  Nil -> error "Key does not exist"
  where i = Helper.findKey k (keys x)

-- splitChild splits the i'th child of a Node into two segments, and the middle
-- element of the child is inserted at the i'th index in the Node. The
-- partitioning is done such that the left and the right segments (y and z
-- respectively) can contain atmost (degree - 1) elements.
splitChild :: (Show a) => Node a -> Int -> IO (Node a)
splitChild x@Node{} i = do
  let childNode = child x !! i
  let k = take i (keys x) ++ (keys childNode !! degree x) : drop i (keys x)
  case childNode of
    c@Node{} -> do
      let y = Node
            { keyCount = degree x
            , degree   = degree x
            , keys     = take (degree x) (keys c)
            , child    = take (degree x) (child c)
            }
      let z = Node
            { keyCount = degree x - 1
            , degree   = degree x
            , keys     = drop (degree x) (keys c)
            , child    = drop (degree x) (child c)
            }
      return Node
        { keyCount = keyCount x + 1
        , degree   = degree x
        , keys     = k
        , child    = take i (child x) ++ y : z : drop (Helper.calcIndex i False)
                                                      (child x)
        }
    c@Leaf{} -> do
      rightLeaf <- genLeafName
      let y = Leaf
            { keyCount = degree x
            , degree   = degree x
            , keys     = take (degree x) (keys c)
            , values   = take (degree x) (values c)
            , next     = Just rightLeaf
            }
      let z = Leaf
            { keyCount = degree x - 1
            , degree   = degree x
            , keys     = drop (degree x) (keys c)
            , values   = drop (degree x) (values c)
            , next     = next c
            }
      let
        ret = Node
          { keyCount = keyCount x + 1
          , degree   = degree x
          , keys     = k
          , child = take i (child x) ++ y : z : drop (Helper.calcIndex i True)
                                                     (child x)
          }
      -- Sync updates to leaves on disk.
      leftLeaf <- getLeafFile y
      Disk.syncNode leftLeaf  y
      Disk.syncNode rightLeaf z
      -- The right leaf (z) doesn't need to be synced to disk as it is
      -- currently empty.
      return ret
    Nil -> error "splitChild: cannot split Nil type"
splitChild Leaf{} _ = error "splitChild: cannot split Leaf type"
splitChild Nil    _ = error "splitChild: cannot split Nil type"

-- insert inserts a key into the BPlusTree.
insert :: (Show a) => Tree a -> Int -> a -> IO (Tree a)
insert t k v
  | keyCount x == 2 * degree x - 1 = do
    let s = Node {keyCount = 0, degree = degree x, keys = [], child = [x]}
    updatedChild <- splitChild s 0
    updatedRoot  <- insertNonFull updatedChild k v
    return Tree {root = updatedRoot}
  | otherwise = do
    updatedRoot <- insertNonFull x k v
    return Tree {root = updatedRoot}
  where x = root t

-- insertNonFull inserts an element into a node having less than 2*(degree) - 1
-- elements.
insertNonFull :: (Show a) => Node a -> Int -> a -> IO (Node a)
insertNonFull x@Node{} k v
  | keyCount (child x !! i) == 2 * degree x - 1 = do
    x1 <- splitChild x i
    let i1 = Helper.calcIndex i (k > (keys x1 !! i))
    updatedChild <- insertNonFull (child x1 !! i1) k v
    return Node
      { keyCount = keyCount x1
      , degree   = degree x1
      , keys     = keys x1
      , child    = take i1 (child x1) ++ updatedChild : drop (i1 + 1) (child x1)
      }
  | otherwise = do
    updatedChild <- insertNonFull (child x !! i) k v
    return Node
      { keyCount = keyCount x
      , degree   = degree x
      , keys     = keys x
      , child    = take i (child x) ++ updatedChild : drop (i + 1) (child x)
      }
  where i = Helper.insertIndex k (keys x) (keyCount x - 1)
insertNonFull x@Leaf{} k v = do
  let i = Helper.insertIndex k (keys x) (keyCount x - 1)
  let ret = Leaf
        { keyCount = keyCount x + 1
        , degree   = degree x
        , keys     = take i (keys x) ++ k : drop i (keys x)
        , values   = take i (values x) ++ v : drop i (values x)
        , next     = next x
        }
  leafName <- getLeafFile ret
  syncNode leafName ret
  return ret
insertNonFull Nil _ _ = error "insertNonFull: cannot insert in Nil type"
