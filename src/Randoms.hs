module Randoms where

import System.Random

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g = let (value, newGen) = random g in value : randoms' newGen

takeFiveRandomWithSeed n = take 5 $ randoms' (mkStdGen n) :: [Int]
takeFiveRandomWithGenerator g = take 5 $ randoms' g :: [Int]
