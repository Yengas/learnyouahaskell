module Functions where

oddSquaresLesserThan1000WithComposition = let
        squaresLesserThan x = (takeWhile (<x) . map (^2)) [1..]
        odds = filter odd
    in
        (odds . squaresLesserThan) 1000

applyExample = show $ map negate $ filter odd $ takeWhile (<1000) $ map (^3) [1..100]
applyExampleWithComposition = (show . map negate . filter odd . takeWhile (<1000) . map (^3)) [1..100]

factorial n = product [1..n]
factorialWithApply n = foldr ($) 1 $ map (*) [1..n]

lambdaExample = foldr (\x acc -> acc + x) 0 [1,2,3,4,5]
lambdaMatchingExample = foldr (\(x, y) acc -> acc + (x * y)) 0 [(1,2), (3,4), (5,6), (7,8), (9,10)]