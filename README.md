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

```
cabal run
```
