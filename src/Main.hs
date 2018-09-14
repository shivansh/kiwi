module Main where

import BPlusTree
import Types

main :: IO ()
main = do
  t  <- BPlusTree.create 3           -- create a B+ Tree of degree 3
  t1 <- BPlusTree.insert t 1 "v1"   -- insert key-value pair {1, "v1"}
  t2 <- BPlusTree.insert t1 2 "v2"
  t3 <- BPlusTree.insert t2 3 "v3"
  t4 <- BPlusTree.insert t3 4 "v4"
  t5 <- BPlusTree.insert t4 5 "v5"
  t6 <- BPlusTree.insert t5 6 "v6"
  print $ search (Types.root t6) 3  -- lookup value corresponding to key 3
