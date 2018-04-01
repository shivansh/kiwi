package main

import "fmt"

type Tree struct {
	root *Node
}

type Node struct {
	n     int  // number of keys currently stored
	t     int  // degree of a node
	leaf  bool // indicates if the node is leaf
	keys  []int
	vals  [][]byte
	child []*Node
}

// Create creates a new BTree of a given degree.
func Create(degree int) *Tree {
	x := &Node{0, degree, true, []int{}, [][]byte{}, []*Node{}}
	t := &Tree{x}
	return t
}

// Search returns the value corresponding the key k if it exists in BTree and -1
// otherwise.
func Search(x *Node, k int) []byte {
	i := 0
	for ; i <= x.n && k > x.keys[i]; i++ {
	}
	if i <= x.n && k == x.keys[i] {
		return x.vals[i]
	} else if x.leaf {
		return []byte{} // TODO: Handle non-existence.
	} else {
		return Search(x.child[i], k)
	}
}

// SplitChild splits the i'th child of a Node into two segments, and the middle
// element of the child is inserted at the i'th index in the Node. The
// partitioning is done such that the left and the right segments (x and y
// respectively) can contain atmost t-1 elements.
func SplitChild(x *Node, i int) {
	y := x.child[i]
	z := &Node{x.t - 1, x.t, y.leaf, []int{}, [][]byte{}, []*Node{}}
	for j := 0; j < x.t-1; j++ {
		z.keys, z.vals = append(z.keys, y.keys[j+x.t]), append(z.vals, y.vals[j+x.t])
	}
	if !y.leaf {
		z.child = []*Node{}
		for j := 0; j < x.t; j++ {
			z.child = append(z.child, y.child[j+x.t])
		}
	}
	y.n = x.t - 1
	c := &Node{0, x.t, IsLeaf(x.child), []int{}, [][]byte{}, []*Node{}}
	x.child = append(x.child, c) // increment size by one
	for j := len(x.child) - 1; j > i+1; j-- {
		x.child[j] = x.child[j-1]
	}
	x.child[i+1] = z
	x.keys, x.vals = append(x.keys, 0), append(x.vals, []byte{})
	// Insert the middle element of the child Node into x at i'th index.
	for j := x.n - 1; j >= i; j-- {
		x.keys[j+1], x.vals[j+1] = x.keys[j], x.vals[j]
	}
	x.keys[i], x.vals[i] = y.keys[x.t-1], y.vals[x.t-1]
	x.n = x.n + 1
	// Update the left segment, making it i'th child of x.
	y.keys, y.vals = y.keys[:x.t-1], y.vals[:x.t-1]
	y.n = len(y.keys)
	if x.t < len(y.child) {
		y.child = y.child[:x.t]
	}
	x.child[i] = y
}

// Insert inserts the key k into the BTree.
func Insert(T *Tree, k int, v []byte) {
	x := T.root
	if x.n == 2*x.t-1 {
		s := &Node{0, x.t, false, []int{}, [][]byte{}, []*Node{x}}
		T.root = s
		SplitChild(s, 0)
		InsertNonFull(s, k, v)
	} else {
		InsertNonFull(x, k, v)
	}
}

// InsertNonFull inserts an element into a node having less than 2*t-1 elements.
func InsertNonFull(x *Node, k int, v []byte) {
	i := x.n - 1
	if x.leaf {
		x.keys, x.vals = append(x.keys, 0), append(x.vals, []byte{})
		for ; i >= 0 && k < x.keys[i]; i-- {
			x.keys[i+1], x.vals[i+1] = x.keys[i], x.vals[i]
		}
		x.keys[i+1], x.vals[i+1] = k, v
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
		InsertNonFull(x.child[i], k, v)
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
	vals := [][]byte{[]byte("hello"), []byte("world")}
	for i := 1; i <= 8; i++ {
		// for k, v := range []int{1, 2, 3, 4, } {
		Insert(t, i, vals[0])
	}
	fmt.Println(string(Search(t.root, 2)))
}
