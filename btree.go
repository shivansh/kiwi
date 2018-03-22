package main

import "fmt"

type Tree struct {
	root *Node
}

type Node struct {
	n     int   // number of keys currently stored
	t     int   // degree of a node
	leaf  bool  // indicates if the node is leaf
	keys  []int // TODO: flexible types
	child []*Node
}

// Create creates a new Tree of a given degree.
func Create(degree int) *Tree {
	x := &Node{0, degree, true, []int{}, []*Node{}}
	t := &Tree{x}
	return t
}

// Search returns the index of the key if it exists in BTree and -1 otherwise.
func Search(x *Node, k int) int {
	i := 0
	for ; i <= x.n && k > x.keys[i]; i++ {
	}
	if i <= x.n && k == x.keys[i] {
		return i
	} else if x.leaf {
		return -1
	} else {
		return Search(x.child[i], k)
	}
}

// SplitChild splits the i'th child of a Node into two segments, and the middle
// element of the child is inserted at the i'th index in the Node. The
// partitioning is done such that the left and the right segments can contain
// atmost t-1 elements.
func SplitChild(x *Node, i int) {
	y := x.child[i]                                      // left segment
	z := &Node{x.t - 1, x.t, y.leaf, []int{}, []*Node{}} // right segment
	for j := 0; j < x.t-1; j++ {
		z.keys = append(z.keys, y.keys[j+x.t])
	}
	if !y.leaf {
		z.child = []*Node{}
		for j := 0; j < x.t; j++ {
			z.child = append(z.child, y.child[j+x.t])
		}
	}
	y.n = x.t - 1
	c := &Node{0, x.t, IsLeaf(x.child), []int{}, []*Node{}}
	x.child = append(x.child, c) // increment size by one
	for j := len(x.child) - 1; j > i+1; j-- {
		x.child[j] = x.child[j-1]
	}
	x.child[i+1] = z
	x.keys = append(x.keys, 0) // increment size by one
	// Insert the middle element of the child Node into x at i'th index.
	for j := x.n - 1; j >= i; j-- {
		x.keys[j+1] = x.keys[j]
	}
	x.keys[i] = y.keys[x.t-1]
	x.n = x.n + 1
	// Update the left segment, making it i'th child of x.
	y.keys = y.keys[:x.t-1]
	y.n = len(y.keys)
	if x.t < len(y.child) {
		y.child = y.child[:x.t]
	}
	x.child[i] = y
}

// Insert inserts the key k into the BTree.
func Insert(T *Tree, k int) {
	x := T.root
	if x.n == 2*x.t-1 {
		s := &Node{0, x.t, false, []int{}, []*Node{x}}
		T.root = s
		SplitChild(s, 0)
		InsertNonFull(s, k)
	} else {
		InsertNonFull(x, k)
	}
}

// InsertNonFull inserts an element into a node having less than 2*t-1 elements.
func InsertNonFull(x *Node, k int) {
	i := x.n - 1
	if x.leaf {
		x.keys = append(x.keys, 0) // increment size by one
		for ; i >= 0 && k < x.keys[i]; i-- {
			x.keys[i+1] = x.keys[i]
		}
		x.keys[i+1] = k
		x.n = x.n + 1
	} else {
		for ; i >= 0 && k < x.keys[i]; i-- {
		}
		i++
		if x.child[i].n == 2*x.t-1 {
			SplitChild(x, i)
			if k > x.keys[i] {
				i++
			}
		}
		InsertNonFull(x.child[i], k)
	}
}

// IsLeaf checks if the given container contains leaf nodes.
func IsLeaf(x []*Node) bool {
	if len(x) > 0 {
		return x[0].leaf
	} else {
		return true
	}
}

// Traverse does a depth-first traversal of the BTree providing a rough sketch
// of its structure.
func Traverse(x *Node, level int) {
	fmt.Printf("level %d\n", level)
	for k, v := range x.keys {
		fmt.Printf("{%d: %d}, ", k, v)
	}
	fmt.Println()
	for _, v := range x.child {
		Traverse(v, level+1)
	}
}

// InOrder does an inorder traversal of the BTree.
func InOrder(x *Node) {
	for k, v := range x.keys {
		if !x.leaf {
			InOrder(x.child[k])
		}
		fmt.Printf("%d, ", v)
	}
	if !x.leaf {
		InOrder(x.child[x.n])
	}
}

func main() {
	t := Create(2)
	for _, v := range []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} {
		Insert(t, v)
	}
	Traverse(t.root, 0)
}
