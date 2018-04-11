// An implementation of disk-based operations supported by a B+ Tree.
package bplustree

import (
	"errors"
	"os"
)

const blockSize = int64(4096)

func (t *Tree) CreateDBFIle() (*os.File, error) {
	var fp *os.File
	if _, err := os.Stat(t.dbPath); os.IsNotExist(err) {
		fp, err = os.Create(t.dbPath)
		if err != nil {
			return nil, err
		}
	}
	return fp, nil
}

// AdjustOffset adjusts the offset to the nearest block boundary. This is to
// ensure that writes corresponding to a leaf start at a new block.
func AdjustOffset(pos int64) int64 {
	var offset int64 = 0
	for ; offset <= pos; offset += blockSize {
	}
	return offset
}

// WriteToDisk writes the values stored in the leaf nodes of a B+ Tree to disk.
// It is currently assumed that a leaf node can occupy atmost one block.
func (t *Tree) WriteToDisk() error {
	x := t.Root
	// Locate the leftmost leaf node.
	for {
		if x.leaf {
			break
		}
		x = x.child[0]
	}
	fp, err := t.CreateDBFIle()
	if err != nil {
		return err
	}
	var offset int64 = 0
	for {
		for _, v := range x.vals {
			// The length of the byte slice preceeds it in the file.
			w := []byte{byte(len(v))}
			w = append(w, v...)
			n, err := fp.Write(w)
			if err != nil {
				return err
			}
			offset += int64(n)
		}
		offset = AdjustOffset(offset)
		fp.Seek(offset, 0)
		if x.next == nil {
			break
		} else {
			x = x.next
		}
	}
	if err = fp.Sync(); err != nil {
		return err
	}
	if err = fp.Close(); err != nil {
		return err
	}
	return nil
}

// Search returns the value present in the leaf node corresponding to the key k.
// TODO: It is currently assumed that all internal nodes are present in memory.
func (t *Tree) Search(x *Node, k int) ([]byte, error) {
	i := x.n - 1
	if x.leaf {
		for ; i >= 0 && x.keys[i] >= k; i-- {
		}
		i++
		// TODO: A better check of existence.
		if i < len(x.keys) && x.keys[i] == k {
			fp, err := os.Open(t.dbPath)
			if err != nil {
				return []byte{}, err
			}
			b, err := t.GetLeafIndex(k)
			if err != nil {
				return []byte{}, err
			}
			vals, err := ReconstructBytesFromFile(fp, b)
			if err != nil {
				return []byte{}, err
			}
			return vals[i], nil
		} else {
			return []byte{}, errors.New("Key not found")
		}
	} else {
		for ; i >= 0 && x.keys[i] >= k; i-- {
		}
		i++
		return t.Search(x.child[i], k)
	}
	return []byte{}, nil
}

// ReconstructBytesFromFile reconstructs a slice of byte slices from the B+ Tree
// file. Each byte slice in the file is preceded by its size.
// b is the index of the block which is to be read.
func ReconstructBytesFromFile(fp *os.File, b int) ([][]byte, error) {
	ret := [][]byte{}
	block := make([]byte, blockSize)
	// Set file offset to point at the beginning of the block.
	fp.Seek(int64(b)*blockSize, 0)
	if _, err := fp.Read(block); err != nil {
		return [][]byte{}, err
	}
	for i := 0; i < len(block); {
		l := int(block[i])
		b := block[i+1 : i+l+1]
		if len(b) > 0 {
			ret = append(ret, b)
		}
		i += l + 2
	}
	return ret, nil
}

// ReconstructIndexFromFile reconstructs the index into memory from disk.
// TODO: Figure out how to store index into disk.
func ReconstructIndexFromFile() {

}

// GetLeafIndex returns the index of the leaf corresponding to the key k.
func (t *Tree) GetLeafIndex(k int) (int, error) {
	m := t.Root.t
	// The ith leaf can have a range of keys given by
	// [(2*m-1)*i, (2*m-1)*i + (m-1)], where m is the degree of the B+ Tree.
	for i := 0; k >= (2*m-1)*i || k >= (2*m-1)*i+m-1; i++ {
		if k >= (2*m-1)*i && k <= (2*m-1)*i+m-1 {
			return i, nil
		}
	}
	return -1, errors.New("Leaf does not exist for the given key")
}
