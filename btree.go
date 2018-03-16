package main

import "fmt"

type key int

type Node struct {
	// TODO make m global ?
	// m is also the overflow limit for a node.
	m    int // maximum number of children a node can have.
	keys []key
	// For a leaf node, children is nil.
	children   []*Node
	childCount int
	parent     *Node
}

var debug = false

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
	if root.childCount == 0 {
		// The current node is a leaf, insert the key here.
		UpdateKeys(root, k, childIndex)
		if len(root.keys) == root.m {
			if debug {
				fmt.Printf("leaf overflowed on adding: %d\n", k)
			}
			HandleOverflow(root)
		}
	} else if root.children[childIndex] != nil {
		Insert(root.children[childIndex], k)
	} else {
		if debug {
			fmt.Printf("childIndex: %d\n", childIndex)
		}
		root.children[childIndex] = &Node{
			root.m,
			[]key{k},
			make([]*Node, root.m), // TODO update to m
			0,
			root,
		}
		root.childCount++
	}
}

// A node is overflowed when len(keys) == m.
func HandleOverflow(n *Node) {
	if n.parent == nil {
		childCount := 0
		childMid := (len(n.children) - 1) / 2
		for _, v := range n.children[0 : childMid+1] {
			if v != nil {
				childCount++
			}
		}
		mid := (len(n.keys) - 1) / 2
		child := make([]*Node, n.m)
		copy(child, n.children[0:childMid+1])
		leftChild := &Node{
			n.m,
			n.keys[0:mid],
			child,
			childCount,
			n,
		}
		childCount = 0
		for _, v := range n.children[childMid+1:] {
			if v != nil {
				childCount++
			}
		}
		child = make([]*Node, n.m)
		copy(child, n.children[childMid+1:])
		rightChild := &Node{
			n.m,
			n.keys[mid+1:],
			child,
			childCount,
			n,
		}
		n.children = make([]*Node, n.m) // TODO
		n.children[0] = leftChild
		n.children[1] = rightChild
		// The modified parent contains only the middle key.
		n.keys = []key{n.keys[mid]}
		n.childCount = 2
	} else {
		// Find the child pointer in parent for the current node.
		index := 0
		for i, child := range n.parent.children {
			if child == n {
				index = i
				break
			}
		}
		mid := (len(n.keys) - 1) / 2
		leftKeys := n.keys[0:mid]
		rightKeys := n.keys[mid+1:]
		childCount := 0
		childMid := (len(n.children) - 1) / 2
		for _, v := range n.children[0 : childMid+1] {
			if v != nil {
				childCount++
			}
		}
		child := make([]*Node, n.m)
		copy(child, n.children[0:childMid+1])
		n.parent.children[index] = &Node{
			n.m,
			leftKeys,
			child,
			childCount,
			n.parent,
		}
		childCount = 0
		for _, v := range n.children[childMid+1:] {
			if v != nil {
				childCount++
			}
		}
		child = make([]*Node, n.m)
		copy(child, n.children[childMid+1:])
		rightChild := &Node{
			n.m,
			rightKeys,
			child,
			childCount,
			n.parent,
		}
		n.parent.keys = append(n.parent.keys, n.keys[mid])
		if debug {
			fmt.Printf("appending to parent: %d\n", n.keys[mid])
			for _, k := range n.parent.keys {
				fmt.Printf("parent key: %d\n", k)
			}
		}
		UpdateChildren(n.parent, rightChild, index+1)
		if len(n.parent.keys) >= n.m {
			// overflow in parent.
			if debug {
				fmt.Println("overflow in parent")
			}
			HandleOverflow(n.parent)
		}
	}
}

func Delete() {}

func UpdateKeys(n *Node, k key, i int) {
	n.keys = append(n.keys, k) // allocate one extra space
	copy(n.keys[i+1:], n.keys[i:])
	n.keys[i] = k
}

func UpdateChildren(n *Node, child *Node, i int) {
	if i < len(n.children) && n.children[i] == nil {
		n.children[i] = child
	} else {
		// TODO: Will this block ever be executed ?
		n.children = append(n.children, child) // allocate one extra space
		copy(n.children[i+1:], n.children[i:])
		n.children[i] = child
	}
	if debug {
		fmt.Printf("len children: %d\n", len(n.children))
	}
}

func Traverse(root *Node, level int) {
	fmt.Printf("Level %d\n", level)
	for k, v := range root.keys {
		fmt.Printf("{%d: %d}, ", k, v)
	}
	fmt.Println("")
	for _, c := range root.children {
		if c != nil {
			Traverse(c, level+1)
		}
	}
}

func main() {
	m := 3
	n := &Node{m, []key{1}, make([]*Node, m), 0, nil}

	for _, i := range []key{2, 3, 4, 5, 6, 7, 8} {
		Insert(n, i)
	}

	Traverse(n, 0)
}
