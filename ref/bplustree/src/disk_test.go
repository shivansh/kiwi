package bplustree

import (
	"reflect"
	"testing"
)

func TestTree_Search(t *testing.T) {
	tree := Create(2, "/tmp/bplusfile")
	vals := [][]byte{[]byte("abc"), []byte("def"), []byte("ghi")}
	for k, v := range vals {
		tree.Insert(k, v)
	}
	tree.WriteToDisk()

	type args struct {
		x *Node
		k int
	}
	tests := []struct {
		name    string
		args    args
		want    []byte
		wantErr bool
	}{{
		name:    "Test for a valid key",
		args:    args{tree.Root, 1},
		want:    []byte("def"),
		wantErr: false,
	}, {
		name:    "Test for an invalid key",
		args:    args{tree.Root, 4},
		want:    []byte(""),
		wantErr: true,
	},
	}
	for _, tt := range tests {
		got, err := tree.Search(tt.args.x, tt.args.k)
		if (err != nil) != tt.wantErr {
			t.Errorf("%q. Tree.Search() error = %v, wantErr %v", tt.name, err, tt.wantErr)
			continue
		}
		if !reflect.DeepEqual(got, tt.want) {
			t.Errorf("%q. Tree.Search() = %v, want %v", tt.name, got, tt.want)
		}
	}
}
