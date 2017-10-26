module Main where
import FizzBuzz
import Comprehensions
import Case
import Recursion
import Functions
import Records
import BinaryTree
import Data.Char
import qualified Data.Map as Map

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

-- main = putStrLn "Hello, World!"