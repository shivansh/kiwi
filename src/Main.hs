module Main where

import BPlusTree
import Disk
import Types

leafName :: DBFile
leafName = "leaf"

main :: IO ()
main = do
    t <- BPlusTree.create 2
    t1 <- BPlusTree.insert t 1
    t2 <- BPlusTree.insert t1 2
    t3 <- BPlusTree.insert t2 3
    t4 <- BPlusTree.insert t3 4
    t5 <- BPlusTree.insert t4 5
    t6 <- BPlusTree.insert t5 6
    t7 <- BPlusTree.insert t6 7
    t8 <- BPlusTree.insert t7 8
    t9 <- BPlusTree.insert t8 9
    t10 <- BPlusTree.insert t9 10
    print 10
