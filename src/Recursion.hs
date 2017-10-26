module Recursion where

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let
        smallerSorted = quickSort' [y | y <- xs, y <= x]
        biggerSorted  = quickSort' [y | y <- xs, y > x]
    in
        smallerSorted ++ [x] ++ biggerSorted

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys