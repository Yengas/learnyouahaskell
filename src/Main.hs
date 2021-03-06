module Main where
import FizzBuzz
import Comprehensions
import Case
import Recursion
import Functions
import Records
import BinaryTree
import Data.Char
import Randoms
import Functors
import Monoid
import Monad
import Writer
import Stack
import ProbMonad
import System.Random
import qualified Data.Map as Map

-- Notes for ghci
-- :info => shows info about the type and interfaces it implements
-- :t    => shows the type of the value
-- :k    => shows what kind of a type a type is. For example Int is a concrete type so :k on it will return Int :: *
--          for Maybe however we require a type to make it concrete so it will be Maybe :: * -> *, also for either;
--          Either :: * -> * -> *, Either String :: * -> *, Either String Int :: *

main :: IO()
-- |The FizzBuzz printing...
-- main = mapM_ putStrLn (map getFizzBuzzSimple [1..100])

-- |Comprehensions printing...
-- main = mapM_ putStrLn (map show comprehensionWithFilterAndLet)
-- main = mapM_ putStrLn (map show comprehensionMultiWithFilter)

-- |Case printing...
-- main = putStrLn (show (getLengthCase' [1..5]))
-- main = putStrLn (showPossibleLength' [1..1])
-- main = putStrLn (showPossibleLengthLetOneLiner' [1..2])

-- |Recursion examples...
-- main = mapM_ putStrLn (map show (quickSort' [9, 7, 5, 3, 11, 42, 73, 1]))
-- main = putStrLn (show (elem' 4 [1, 7, 5, 3]))

-- below functions requires some describing. its kinda hard to get. The deal is, zipWith' is a function that takes
-- 3 types a, b, c where we give it a function to compose A and B types into C. Then it goes over a list of A and a
-- list of B to create a list of C. When we give a zipWith' (+) function to zipWith' it means that the composer function
-- is a function that takes a 2 dimensional list of A and a 2 dimensional list of B to create a 2 dimensional list of Cs
-- so the zipWith now can zip [1,2,3] with [4,5,6] and [3,4,5] with [10,10,10] with a call to the newly created composer
-- with scala signatures: zipWith' (+) returns: (Seq[A], Seq[B]) => Seq[C] and since we have a list of Seq[Seq[Int]]
-- everything works out as expected.
-- main = mapM_ putStrLn (map show (zipWith' (zipWith' (+)) [[1,2,3], [3,4,5]] [[4,5,6], [10,10,10]]))

-- |Function related stuff...
-- main = putStrLn $ show oddSquaresLesserThan1000WithComposition
-- main = putStrLn $ show $ factorialWithApply 6
-- main = putStrLn $ applyExampleWithComposition

-- |Records related stuff...
-- main = let person = Person "Yigitcan" "uCuM" in putStrLn $ upperCasedFullName person
-- main = let person = Person "Yiğitcan" "uÇuM" in putStrLn $ upperCasedFullNameWithDataChar person
-- main = putStrLn $ show $ exampleYigitNamed
-- main = let person = Person { lastName = "Doe", firstName = "John" } in putStrLn $ fullNameNamed person
-- main = putStrLn $ fullNameNamed exampleYigit
-- main = let vector1 = Vector 1 2 3; vector2 = Vector 3 4 5 in putStrLn $ show $ vector1 `vectorAdd` vector2
-- main = putStrLn $ show $ [(read "Sali") .. Cuma]
-- main = let lockerRoom = Map.fromList [(1, (Free, "HELLO")), (7, (Used, "WORLD"))] in putStrLn $ show $ getCodeForLocker 3 lockerRoom

-- |BinaryTree related stuff...
-- main = let x = insert 4 $ insert 9 $ insert 7 $ insert 3 $ singleton 5 in putStrLn $ show $ x where insert = flip treeInsert
-- main = let tree = treeFromList [1,2,3,4] in putStrLn $ show $ tree
-- main = let tree = treeFromList [1,5,3,7,4,8,2,9,6,0] in putStrLn $ "5 in tree: " ++ (show $ tree `treeElem` 5) ++  ", 10 in tree: " ++ (show $ tree `treeElem` 10)
-- below doesnt obey the functor rules because binary tree changes how it behaves/holds data.
-- it needs to be implemented in a way that will hold the data in an ordered manner, no matter which functions is
-- passed to the functor.
-- main = let tree = treeFromList [1,2,3] in putStrLn $ show $ fmap (\x -> 1 / x) tree

-- |Random related stuff...
-- main = mapM_ (putStrLn . show) $ takeFiveRandomWithSeed 73
-- main = do
--    stdGen <- getStdGen
--    mapM_ (putStrLn . show) $ takeFiveRandomWithGenerator stdGen
-- main = print $ takeTenWithStateDry

-- |Functors and Applicative Functors related stuff
-- main = reverseIntersperseUpperLines
-- main = reverseIntersperseUpperLinesOneLiner
-- main = putStrLn $ show $ functionComposition 5
-- main = print $ replicateExample
-- main = print $ justApplicative (Just 5)
-- main = print $ justMultipleApplicative (Just 5) (Just 8)
-- main = print $ justMultipleApplicative (Nothing) (Just 8)
-- main = print $ justApplicativeLifted (Just 5)
-- main = print $ justApplicativeFunc (Just 5) (Just 6)
-- main = print $ listApplicativeMultipleExample
-- main = print $ listApplicativeMultipleTimesExample
-- main = print $ listPossibleProductsExample [2, 5, 10] [8, 10, 11]
-- main = ioApplicativeExample
-- main = print $ functionCompositionAnswerToLife
-- main = print $ zipListApplicativeExample
-- main = print $ sequence' [Just 1, Just 2, Just 3]
-- main = print $ conditionsSequenceExample 5

-- |Monoid examples
-- main = print $ foldTreeExample
-- main = print $ monoidTestExample 5
-- main = print $ foldrFactorial 7
-- main = print $ treeToArrayExample
-- main = print $ lengthCompare "yakutcan" "yiğit"

-- |Monad examples
-- main = print $ addBirds (0, 0) [(L, 2), (L, 2), (R, 7), (R, 1), (L, 10)]
-- main = print $ birdAddMonadExample
-- main = ignoreResultPrintExample
-- main = print $ doWithListMonadExample
-- main = do
--    print $ doWithMaybeMonadExample (Just (3, 4))
--    print $ doWithMaybeMonadExample Nothing
-- main = putStr "Do With Function: " >> (print $ doWithFunctionExample 5) >>
--    putStr "Lambda  Function: " >> (print $ lambdaMonadFunctionExample 5)
-- main = print $ lambdaScopeExample
-- main = print $ moveKnight (6, 2)
-- main = print $ (6,2) `canReachIn3` (6,1)
-- main = print $ let cIn3 = canReachIn 3 in (6,2) `cIn3` (7,3)
-- main = print $ liftMExample

-- |Writer examples
-- main = print $ addDrinkExample
-- main = multWithLogExample
-- main = let (r, logs) = gcdExample in mapM_ putStrLn (logs ++ ["Result: " ++ (show r)])

-- |Stack examples with custom/standard monad state
-- main = print $ customStateExample
-- main = print $ customStateWithDoExample

-- |Prob example for monads
-- main = print $ negateExample
-- main = print $ flattenExample
-- main = print $ applicativeFunctorExample
main = print $ combinePos flipThreeHeadsPossibility

-- main = putStrLn "Hello, World!"
