data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

-- insert a key into a BST.
insert :: (Ord a) => a -> Tree a -> Tree a
insert k (Node n lt rt)
  | k <= n && lt == Nil = Node n (Node k Nil Nil) rt
  | k <= n = Node n (insert k lt) rt
  | k > n && rt == Nil = Node n lt (Node k Nil Nil)
  | otherwise = Node n lt (insert k rt)

-- Delete a key from BST.
-- Assumptions:
--   * BST has unique elements
-- delete finds the node to be deleted in BST.
delete :: (Ord a) => a -> Tree a -> Tree a
delete k (Node n lt rt)
  | k /= n && (lt, rt) == (Nil, Nil) = (Node n lt rt)
  | k == n = delNode k (Node n lt rt)
  | k < n = Node n (delete k lt) rt
  | otherwise = Node n lt (delete k rt)

-- delNode performs the delete operation on the given node.
delNode :: (Ord a) => a -> Tree a -> Tree a
delNode k (Node n lt rt)
  | (lt, rt) == (Nil, Nil) = Nil -- leaf
  | lt == Nil = rt
  | rt == Nil = lt
  | otherwise = (Node (inorderSucc k rt)) lt (delete (inorderSucc k rt) rt)

-- inorderSucc returns the inorder successor of a node in BST.
inorderSucc :: (Ord a) => a -> Tree a -> a
inorderSucc k (Node n lt rt)
  | lt == Nil = n
  | otherwise = inorderSucc k lt

-- find a key in a BST
find :: (Ord a) => a -> Tree a -> Bool
find k (Node n lt rt)
  | k == n = True
  | (lt, rt) == (Nil, Nil) = False
  | k < n = find k lt
  | otherwise = find k rt
