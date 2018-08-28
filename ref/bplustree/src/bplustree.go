// An implementation of in-memory operations supported by a B+ Tree.

package bplustree

import "fmt"

type Tree struct {
	Root   *Node
	dbPath string
	index  map[int]int
}

type Node struct {
	n     int  // number of keys currently stored
	t     int  // degree of a node. TODO: Rename to m.
	leaf  bool // indicates if the node is a leaf
	keys  []int
	vals  [][]byte // only leaf nodes will contain values
	child []*Node
	next  *Node // pointer to the next leaf node (if current node is a leaf)
}

// Create creates a new B+ Tree of given degree.
func Create(degree int, dbPath string) *Tree {
	x := &Node{0, degree, true, []int{}, [][]byte{}, []*Node{}, nil}
	return &Tree{x, dbPath, map[int]int{}}
}

func SplitChild(x *Node, i int) {
	var next *Node
	// TODO: A better check of existence. The problem at the moment is that
	// non-existent children are left unallocated, thus cannot be indexed.
	// This check is likely to fail when random key insertions are done.
	if len(x.child) > i+1 {
		next = x.child[i+1]
	}
	y := x.child[i]
	z := &Node{x.t - 1, x.t, y.leaf, []int{}, [][]byte{}, []*Node{}, next}
	if !y.leaf {
		z.child = []*Node{}
		for j := 0; j < x.t; j++ {
			z.child = append(z.child, y.child[j+x.t])
		}
	} else {
		// Values are only added at the leaves.
		for j := 0; j < x.t-1; j++ {
			z.vals = append(z.vals, y.vals[j+x.t])
		}
	}
	for j := 0; j < x.t-1; j++ {
		z.keys = append(z.keys, y.keys[j+x.t])
	}
	y.n = x.t - 1
	c := &Node{0, x.t, IsLeaf(x.child), []int{}, [][]byte{}, []*Node{}, nil}
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
	y.keys = y.keys[:x.t]
	if y.leaf {
		y.vals = y.vals[:x.t]
	}
	y.n = len(y.keys)
	if x.t < len(y.child) {
		y.child = y.child[:x.t]
	}
	y.next = z
	x.child[i] = y
}

// Insert inserts the value v corresponding to the key k into the B+ Tree.
func (t *Tree) Insert(k int, v []byte) {
	x := t.Root
	if x.n == 2*x.t-1 {
		s := &Node{0, x.t, false, []int{}, [][]byte{}, []*Node{x}, nil}
		t.Root = s
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
		// Values are inserted only at the leaves.
		// TODO: Sync updates to disk.
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

// Traverse does a depth-first traversal of the B+ Tree providing a rough sketch
// of its structure.
func Traverse(x *Node, level int) {
	fmt.Printf("level %d\nkeys: ", level)
	for k, v := range x.keys {
		fmt.Printf("{%d: %d}, ", k, v)
	}
	fmt.Printf("\nvals: ")
	if x.leaf {
		for k, v := range x.vals {
			fmt.Printf("{%d: %s}, ", k, string(v))
		}
	}
	fmt.Println("\n")
	for _, v := range x.child {
		Traverse(v, level+1)
	}
}

// TraverseLeaves linearly traverses all the leaves starting from left. This
// kind of traversal gives the block representation of the data indexed by the
// tree.
func TraverseLeaves(x *Node) {
	if x.leaf {
		for k, v := range x.keys {
			fmt.Printf("{%d: %d}, ", k, v)
		}
		fmt.Println()
		for k, v := range x.vals {
			fmt.Printf("{%d: %s}, ", k, string(v))
		}
		fmt.Println("\n")
		if x.next != nil {
			TraverseLeaves(x.next)
		}
	} else {
		TraverseLeaves(x.child[0])
	}
}
