module Randoms where

import System.Random
import Control.Monad.State

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g = let (value, newGen) = random g in value : randoms' newGen

takeFiveRandomWithSeed n = take 5 $ randoms' (mkStdGen n) :: [Int]
takeFiveRandomWithGenerator g = take 5 $ randoms' g :: [Int]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

takeFiveWithStateMonad :: State StdGen [Int]
-- could use [randomSt | _ <- [1..5]] as well instead of replicate 5 randomSt
takeFiveWithStateMonad = fmap (map (+5)) $ sequence $ replicate 5 randomSt

takeFiveWithStateWithDo :: State StdGen [Int]
takeFiveWithStateWithDo = do
    numbers <- sequence $ replicate 5 randomSt
    return $ map (+5) numbers

-- Take five with the monad and do examples and concatenate them together
takeTenWithState = (fst $ runState takeFiveWithStateMonad (mkStdGen 73)) ++ (fst $ runState takeFiveWithStateWithDo (mkStdGen 73))
-- Create an array of states, runState over them with the same rng, concat the results of computations.
takeTenWithStateDry = concat $ map (\s -> fst $ runState s (mkStdGen 73)) [takeFiveWithStateMonad, takeFiveWithStateWithDo]
