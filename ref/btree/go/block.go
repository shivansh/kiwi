package main

// BlockIndex returns the index of the block where the given key is available.
// func GetLeafIndex(k int, x *Node) int {
//         i := x.n - 1
//         for ; i >= 0 && k <= x.keys[i]; i-- {
//         }
//         i++
//         if i < x.n && k == x.keys[i] {
//                 return x.block
//         } else if !x.leaf {
//                 return GetLeafIndex(k, x.child[i])
//         } else {
//                 return -1
//         }
// }
