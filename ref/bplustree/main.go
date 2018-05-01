package main

import (
	"fmt"
	"goBTree/ref/bplustree/src"
)

func main() {
	t := bplustree.Create(20, "data/bplusfile")
	for k := 0; k < 10000; k++ {
		val := []byte(fmt.Sprintf("v%d", k))
		t.Insert(k, val)
	}
	t.WriteToDisk()
}
