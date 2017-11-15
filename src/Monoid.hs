module Monoid where

import BinaryTree
import qualified Data.Foldable as F
import Data.Monoid

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend ` F.foldMap f r

testTree = Node 5
             (Node 3
               (Node 1 EmptyTree EmptyTree)
               (Node 6 EmptyTree EmptyTree)
             )
             (Node 9
               (Node 8 EmptyTree EmptyTree)
               (Node 10 EmptyTree EmptyTree)
             )

foldTreeExample = F.foldl (+) 0 testTree
treeToArrayExample = F.foldMap (\x -> [x]) testTree

monoidTestExample n = getAll $ mconcat $ map (All) $ sequence [(>4), (<10), odd] n

foldrFactorial n = F.foldr (*) 1 [2..n]

lengthCompare :: String -> String -> Ordering
lengthCompare x y = compare (length x) (length y) `mappend` compare x y