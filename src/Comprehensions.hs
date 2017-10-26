module Comprehensions where

onlyComprehension = [x | x <- [1..100]]
comprehensionWithFilter = [x | x <- [1..100], x `mod` 2 == 0]
comprehensionWithFilterAndLet = [let y = x * 2 in y + 1 | x <- [1..100], x `mod` 2 == 0]
comprehensionWithFilterAndLetAfter = [y | x <- [1..100], x `mod` 2 == 0, let y = x * 2 + 1]

comprehensionMultiWithFilter = [(x, y) | x <- [1..9], y <- [x..9], x > 5]

