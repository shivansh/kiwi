module Types where

-- DBFile defines the filename type of a leaf of B+ Tree. The filenames for a
-- leaf is of the form "l2".
type DBFile = String

-- https://stackoverflow.com/a/12064372/5107319
newtype Tree a = Tree
    { root :: Node a
    } deriving (Show)

-- TODO: Support polymorphic keys.
data Node a
    = Nil
    | Node { keyCount :: Int -- number of keys currently stored
           , degree :: Int -- degree of a node
           , isLeaf :: Bool -- indicates if the node is leaf
           , keys :: [Int]
           , values :: [a]
           , child :: [Node a]
           , next :: Maybe DBFile -- filename of the next leaf node
                                  -- (if current node is a leaf)
            }
    deriving (Show, Read)

newtype MetaData = MetaData
    { leafCount :: Int
    } deriving (Show, Read)
