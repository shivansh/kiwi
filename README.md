# Persistent key-value store
The aim of this project is to implement a persistent key-value store in Haskell and Go, and compare the two implementations based on metrics such as performance, memory usage and challenges encountered during the implementation.

Made as a course project for Functional Programming (CS653).

#### [Slides from the demo](https://docs.google.com/presentation/d/1njLMKtE5ZgBaDsso-NFdqB3-oWEyaF6pQHVgjB5j9rw/edit#slide=id.g38f82bf01d_1_47)

## Directory structure

```
├── ref ...................... Reference implementations
│   ├── bplustree
│   │   └── .................. B+ Tree implementation in Go
│   └── btree
│       └── .................. B Tree implementation in Haskell and Go
└── src
    └── ...................... B+ Tree based persistent key-value store in Haskell
```

## Instructions
From the project root, execute -

```shell
cabal build
```

## Usage
```haskell
import BPlusTree
import Types

main :: IO ()
main = do
    t <- BPlusTree.create 3           -- create a B+ Tree of degree 3
    t1 <- BPlusTree.insert t 1 "v1"   -- insert key-value pair {1, "v1"}
    t2 <- BPlusTree.insert t1 2 "v2"
    t3 <- BPlusTree.insert t2 3 "v3"
    t4 <- BPlusTree.insert t3 4 "v4"
    t5 <- BPlusTree.insert t4 5 "v5"
    t6 <- BPlusTree.insert t5 6 "v6"
    print $ search (Types.root t6) 3  -- lookup value corresponding to key 3
```
