module Monoid where

import BinaryTree
import qualified Data.Foldable as F
import Data.Monoid

-- Monoids are types that implement the Monoid interface which has the following properties;
-- mempty :: m            | an identity value that doesn't change a value when appended to it
-- mappend :: m -> m -> m | a function that takes two monoid elements and merges them into one, for id element, the other value is returned
-- mconcat :: [m] -> m    | concatenates a list of monoid values into a single one. uses the default implementation: foldr mappend mempty
-- For example Ordering is a Monoid, however since types like Bool, Int can have multiple Monoids, they are
-- implemented as monoids using different types. Such as;
-- Any(mappend as OR, mempty as False), All(mappend as AND, mempty as True)
-- Sum(mappend as + , mempty as 0),     Product(mappend as *, mempty as 1)

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