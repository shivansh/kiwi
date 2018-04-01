package main

import "fmt"

type Tree struct {
	root *Node
}

type Node struct {
	n     int  // number of keys currently stored
	t     int  // degree of a node
	leaf  bool // indicates if the node is a leaf
	keys  []int
	vals  [][]byte // only a leaf node will have these
	child []*Node
	next  *Node // pointer to the next leaf node, if a leaf
}

func Create(degree int) *Tree {
	x := &Node{0, degree, true, []int{}, [][]byte{}, []*Node{}, nil}
	return &Tree{x}
}

func SplitChild(x *Node, i int) {
	var next *Node
	if len(x.child) > i+1 {
		next = x.child[i+1]
	}
	y := x.child[i]
	z := &Node{x.t - 1, x.t, y.leaf, []int{}, [][]byte{}, []*Node{}, next}
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
	c := &Node{0, x.t, IsLeaf(x.child), []int{}, [][]byte{}, []*Node{}, nil}
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
	y.keys, y.vals = y.keys[:x.t], y.vals[:x.t]
	y.n = len(y.keys)
	if x.t < len(y.child) {
		y.child = y.child[:x.t]
	}
	y.next = z
	x.child[i] = y
}

// Insert inserts the value v corresponding to the key k into the B+ Tree.
func Insert(T *Tree, k int, v []byte) {
	x := T.root
	if x.n == 2*x.t-1 {
		s := &Node{0, x.t, false, []int{}, [][]byte{}, []*Node{x}, nil}
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
		// Values are inserted only at the leaves.
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
	fmt.Printf("level %d\n", level)
	for k, v := range x.keys {
		fmt.Printf("{%d: %d}, ", k, v)
	}
	fmt.Println()
	for k, v := range x.vals {
		fmt.Printf("{%d: %s}, ", k, string(v))
	}
	fmt.Println("\n")
	for _, v := range x.child {
		Traverse(v, level+1)
	}
}

// TraverseLeaves linearly traverses all the leaves starting from the left.
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

func main() {
	t := Create(2)
	vals := [][]byte{[]byte("hello"), []byte("world")}
	for i := 0; i < 4; i++ {
		Insert(t, i, vals[i%2])
	}
	Traverse(t.root, 0)
}
