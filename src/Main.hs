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
    print t4
