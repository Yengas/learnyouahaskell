module Functors where
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative

-- For functors the signature is;
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x
-- also: (->) r is defined as fmap = (.)

-- There are two rules for a functor.
-- 1-) Applying a function to a functor, should have the same result with applying the function to the value inside the functor
-- then wrapping it with the functor. E.g. fmap func (functor a) == functor (func a)
-- 2-) Applying a function composition over a functor, should have the same result with applying each function of the
-- composition individually to the functor. E.g. fmap f . fmap g = fmap (f.g)
-- For the BinaryTree example we have, the functor is not a valid one, because the property of the tree is to be sorted
-- however if we map it with a function that reverses the sorting, BinaryTree will be invalid.

reverseIntersperseUpperLines = do
    lines <- fmap (map (intersperse '-' . reverse . map toUpper) . lines) getContents
    mapM_ putStrLn lines

reverseIntersperseUpperLinesOneLiner = join $ fmap (mapM_ putStrLn . map (intersperse '-' . reverse . map toUpper) . lines) getContents

functionComposition :: (Num a) => a -> a
functionComposition = fmap (*5) (+3)
-- or = (*5) . (+3)

-- this is also called lifting.
replicateCurried :: (Functor f) => f a -> f [a]
replicateCurried = fmap (replicate 3)

replicateExample :: (Num a) => [[a]]
replicateExample = replicateCurried [1, 2, 3, 4]

-- For applicative functors, we have two functions;
-- pure :: (Functor f) => a -> f a
-- <*> :: (Functor f) => f (a -> b) -> f a -> f b
-- For Maybe, pure is Just, for IO it is return
-- pure f <*> x == fmap f x

-- applicative functor laws
-- pure f <*> x = fmap f x
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u



-- creates a function that multiplies a number with 2 and adds 3 to it. wraps it in pure, then uses applicative functor
-- property to map a Maybe variable over the function.
justApplicative x = pure ((+3) . (*2)) <*> x
-- adds the given two variables in the type of Maybe. this is what you can't do with fmap only.
justMultipleApplicative x y = pure (+) <*> x <*> y
-- multiplies the given variable in the type Maybe with 5
justApplicativeLifted x = (fmap (*) (Just 5)) <*> x
-- Shows a possible way of using fmap and the applicative functor with parameters wrapped in functors,
-- like a normal function that works on values
justApplicativeFunc x y = (+) <$> x <*> y

-- This will result in a list with 8 elements because the left side could be different functions, that needed to be applied
-- to the right handside, which has different values.
listApplicativeExample = (*) <$> [1,2,3,4] <*> [1,2]
-- This is an example where we have multiple functions in the context of array.
listApplicativeMultipleExample = [id, (+1), (\x -> 1 / x)] <*> [1, 2]
-- Could use applicative functors twice
listApplicativeMultipleTimesExample = [(*), (+)] <*> [1, 2] <*> [3, 4]
-- All possible products of two arrays
listPossibleProductsExample x y = (*) <$> x <*> y

-- Concatenate two io string into one with applicative functors.
ioApplicativeExample = join $ (return putStrLn) <*> ((++) <$> getLine <*> getLine)

-- This functions returns the answer to the universe
-- you can think of this applicative functor as binding functions outputs to first function's parameters.
--   f <*> g = \x -> f x (g x)
-- this is what it exactly does. Since the type of the <*> is (Functor f) -> f (a -> b) -> f a -> f b
-- for a function, below is fmap (f) g so it becomes \x y -> f (g x) y. The +1 is bind to the first parameter, but we
-- still need to give it 2 parameters for the function to execute. When we use the applicative functor, the parameter count
-- is decreased by one, because of the way it works. Now the function becomes for: (\x y -> f (g x) y) <*> q =>
-- \x -> f (g x) (q x) the take away here is the parameter count is decreased only when we use the applicative functor.
-- Another fmap would make this function \x -> f (g (w x)) (q (w x)) Try it out for yourself.
functionCompositionAnswerToLife = (\x y -> (x + 1) * (y + 2)) <$> (+1) <*> (subtract 1) $ 5

-- ZipList example for a list application factor that behaves like a zipWith
zipListApplicativeExample = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- defined in Control.Applicative this function is a good example how applicative functors are useful
-- with the normal functors, we can only apply functions over a single functor. with applicative functors, its possible
-- to work with multiple functors

-- custom sequence function that converts an array of functors to an functor of array
sequence' :: (Applicative f) => [f a] -> f [a]
sequence' [] = pure []
sequence' (x:xs) = (:) <$> x <*> (sequence' xs)

-- Check if a given number holds all the conditions
conditionsSequenceExample x = and $ sequence' [(>4), (<10), odd] $ x