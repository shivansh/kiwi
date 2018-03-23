module Main where

import BTree

main :: IO ()
main = do
    let t = BTree.create 2
    let t1 = BTree.insert t 1
    let t2 = BTree.insert t1 2
    let t3 = BTree.insert t2 3
    let t4 = BTree.insert t3 4
    let t5 = BTree.insert t4 5
    let t6 = BTree.insert t5 6
    print t6
