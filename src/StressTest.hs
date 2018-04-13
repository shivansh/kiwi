module StressTest where

import BPlusTree

-- stressTest performs n number of insertions (given as argument) into a B+ Tree
-- and evaluates the overall read/write performance.
stressTest :: Int -> Int -> IO ()
stressTest m limit = do
    t <- BPlusTree.create m
    stressTest' t 1 "v1"
  where
    stressTest' t k v
        | k == limit = print "Exited"
        | otherwise = do
            t1 <- BPlusTree.insert t k v
            stressTest' t1 (k + 1) ("v" ++ show (k + 1))
