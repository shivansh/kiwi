package main

import "fmt"

type key int

type Node struct {
	// TODO make m global ?
	// m is also the overflow limit for a node.
	m    int // maximum number of children a node can have.
	keys []key
	// For a leaf node, children is nil.
	children []*Node
	parent   *Node
}

// BinarySearch returns -1 if v is found in keys, else
// returns the index of the of the child node. Note that
// this index also serves the purpose when dealing with
// a leaf node.
func BinarySearch(keys []key, k key) (childIndex int) {
	li := 0
	ri := len(keys) - 1
	mid := 0

	for li <= ri {
		mid = li + (ri-li)/2
		if k == keys[mid] {
			return -1
		} else if k < keys[mid] {
			ri = mid - 1
		} else {
			li = mid + 1
		}
	}
	if k < keys[mid] {
		return mid
	} else {
		return mid + 1
	}
	return
}

// Search looks up the key in btree and returns true if found.
func Search(root *Node, k key) bool {
	// Perform a binary search on the keys in current node.
	// For nth key, the choice of choosing a child is as follows:
	//	* if (lookup < nth key), follow nth child pointer
	//	* otherwise follow (n+1)th child pointer
	childIndex := BinarySearch(root.keys, k)
	if childIndex == -1 {
		return true
	} else if root.children[childIndex] != nil {
		return Search(root.children[childIndex], k)
	} else {
		return false
	}
}

func Insert(root *Node, k key) {
	// TODO When a node is overflowed, the median will be inserted into the
	// parent node, leading to an O(n) operation (for this single step). A
	// workaround can be to allocate keys in heap but then the btree will
	// not be cache optimized anymore.
	childIndex := 0
	if len(root.keys) != 0 {
		childIndex = BinarySearch(root.keys, k)
	}
	if len(root.children) == 0 {
		// The current node is a leaf, insert the key here.
		UpdateKeys(root, k, childIndex)
		if len(root.keys) == root.m {
			HandleOverflow(root)
		}
	} else if root.children[childIndex] != nil {
		Insert(root.children[childIndex], k)
	} else {
		root.children[childIndex] = &Node{
			root.m,
			[]key{k},
			nil,
			root,
		}
	}
}

// A node is overflowed when len(keys) == m.
func HandleOverflow(n *Node) {
	if n.parent == nil {
		mid := (len(n.keys) - 1) / 2
		leftChild := &Node{
			n.m,
			n.keys[0:mid],
			[]*Node{},
			n,
		}
		n.children = append(n.children, leftChild)
		rightChild := &Node{
			n.m,
			n.keys[mid+1:],
			[]*Node{},
			n,
		}
		n.children = append(n.children, rightChild)
		// The modified parent contains only the middle key.
		n.keys = []key{n.keys[mid]}
	} else {
		// Find the child pointer in parent for the current node.
		index := 0
		for i, child := range n.parent.children {
			if child == n {
				index = i
				break
			}
		}
		mid := len(n.keys) / 2
		leftKeys := n.keys[0:mid]
		rightKeys := n.keys[mid:]
		n.parent.children[index] = &Node{
			n.m,
			leftKeys,
			[]*Node{},
			n,
		}
		rightChild := &Node{
			n.m,
			rightKeys,
			[]*Node{},
			n,
		}
		UpdateChildren(n.parent, rightChild, index+1)
	}
}

func Delete() {}

func UpdateKeys(n *Node, k key, i int) {
	n.keys = append(n.keys, k) // allocate one extra space
	copy(n.keys[i+1:], n.keys[i:])
	n.keys[i] = k
}

func UpdateChildren(n *Node, child *Node, i int) {
	n.children = append(n.children, child) // allocate one extra space
	copy(n.children[i+1:], n.children[i:])
	n.children[i] = child
}

func main() {
	n := &Node{4, []key{1}, []*Node{}, nil}

	for _, i := range []key{4, 3, 5, 0} {
		Insert(n, i)
	}

	fmt.Print("Root: ")
	for _, v := range n.keys {
		fmt.Println(v)
	}
	for _, c := range n.children {
		fmt.Printf("Child: ")
		for _, k := range c.keys {
			fmt.Printf("%d, ", k)
		}
		fmt.Println("")
	}
}
