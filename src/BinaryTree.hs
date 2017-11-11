module BinaryTree where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x (EmptyTree) (EmptyTree)

treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert EmptyTree x = singleton x
treeInsert (Node a left right) x
    | x == a = Node x left right
    | x < a  = Node a (treeInsert left x) right
    | x > a  = Node a left (treeInsert right x)

treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem EmptyTree x = False
treeElem (Node a left right) x
    | x == a = True
    | x < a  = left `treeElem` x
    | x > a  = right `treeElem` x

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList list = foldl treeInsert EmptyTree list

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
