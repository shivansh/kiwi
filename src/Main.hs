module Main where

import BPlusTree
import Types

main :: IO ()
main = do
    t <- BPlusTree.create 3
    t1 <- BPlusTree.insert t 1 "v1"
    t2 <- BPlusTree.insert t1 2 "v2"
    t3 <- BPlusTree.insert t2 3 "v3"
    t4 <- BPlusTree.insert t3 4 "v4"
    t5 <- BPlusTree.insert t4 5 "v5"
    t6 <- BPlusTree.insert t5 6 "v6"
    -- t7 <- BPlusTree.insert t6 7 "v7"
    -- t8 <- BPlusTree.insert t7 8 "v8"
    -- t9 <- BPlusTree.insert t8 9 "v9"
    -- t10 <- BPlusTree.insert t9 10 "v10"
    -- t11 <- BPlusTree.insert t10 11 "v11"
    print $ search (Types.root t6) 3
