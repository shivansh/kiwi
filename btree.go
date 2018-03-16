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

func SplitChild(x *Node, i int) {
	// y is the left segment and z is the right segment, both containing
	// atmost t-1 elements.
	y := x.child[i]
	z := &Node{x.t - 1, x.t, y.leaf, []int{}, []*Node{}}
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
	x.keys = append(x.keys, 0) // increase size by one
	for j := x.n - 1; j >= i; j-- {
		x.keys[j+1] = x.keys[j]
	}
	x.keys[i] = y.keys[x.t]
	x.n = x.n + 1
	y.keys = y.keys[:x.t]
}

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

func InsertNonFull(x *Node, k int) {
	i := x.n - 1
	if x.leaf {
		x.keys = append(x.keys, 0) // increase size by one
		for ; i >= 0 && k < x.keys[i]; i-- {
			x.keys[i+1] = x.keys[i]
		}
		x.keys[i+1] = k
		x.n = x.n + 1
	} else {
		for ; i >= 0 && k < x.keys[i]; i-- {
		}
		i++
		// Allocate space for **to-be** inserted child elements.
		l := len(x.child) - i
		if l <= 0 {
			c := &Node{0, x.t, IsLeaf(x.child), []int{}, []*Node{}}
			for j := 0; j < l+1; j++ {
				x.child = append(x.child, c)
			}
		}
		if x.child[i].n == 2*x.t-1 {
			SplitChild(x, i)
			if k > x.keys[i] {
				i++
			}
		}
		// Allocate space for **to-be** inserted child elements.
		l = len(x.child) - i
		if l <= 0 {
			c := &Node{0, x.t, IsLeaf(x.child), []int{}, []*Node{}}
			for j := 0; j < l+1; j++ {
				x.child = append(x.child, c)
			}
		}
		InsertNonFull(x.child[i], k)
	}
}

// IsLeaf checks if the child container contains leaf nodes.
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

func InOrder(x *Node) {

}

func main() {
	t := Create(2)
	for _, v := range []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} {
		Insert(t, v)
	}
	Traverse(t.root, 0)
}
