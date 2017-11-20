module ProbMonad where
import Data.Ratio
import Control.Monad
import Control.Applicative
import Data.List
import Data.Function

-- The problem is to model a type of list, that holds data with their associated probability
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> ((f x), p)) xs

instance Monad Prob where
  return x = Prob [(x, 1 % 1)]
  m >>= f = flatten (fmap f m)
  fail _ = Prob []

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  -- Using the fmap hack to implement an applicative functor.
  mf <*> m = do
    f <- mf
    x <- m
    return $ f x

thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [(Prob [('y', 1 % 2), ('i', 1 % 2)], 1 % 4)
  ,(Prob [('g', 1 % 2), ('t', 1 % 2)], 3 % 4)
  ]

-- Checks if the probabilities add up to 1
checkIsValid :: Prob a -> Bool
checkIsValid (Prob xs) = 1 == (sum $ map snd xs)

-- Flattens a given list of probability of probabilities list, converts them to 1
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multWithPos xs
  where multWithPos (Prob inner, p) = map (\(x, r) -> (x, r * p)) inner

-- Combines multiple possibilities belonging to the same type to one.
combinePos :: (Eq a) => Prob a -> Prob a
combinePos (Prob xs) = Prob $ map (\xs@(x:_) -> (fst x, sum $ map snd xs)) $ groupBy ((==) `on` fst) xs

negateExample = fmap negate (Prob [(1, 1%4), (73, 2%4), (142, 1%4)])
flattenExample = flatten thisSituation
applicativeFunctorExample = Prob [((\x -> x), 1 % 2), ((\x -> x + 1), 1 % 2)] <*> Prob [(1, 1 % 2), (2, 1 % 2)]

-- Coin and coin examples
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 42 % 73), (Tails, 31 % 73)]

flipThreeHeadsPossibility :: Prob Bool
flipThreeHeadsPossibility = do
  x <- coin
  y <- coin
  z <- loadedCoin
  return (all (==Heads) [x, y, z])

