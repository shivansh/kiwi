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
    let t7 = BTree.insert t6 7
    let t8 = BTree.insert t7 8
    let t9 = BTree.insert t8 9
    let t10 = BTree.insert t9 10
    print t9
