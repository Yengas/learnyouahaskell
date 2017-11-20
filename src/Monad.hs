module Monad where
import Control.Monad

-- Monads are for interacting with functions that return functor values. By using monads, you can chain function calls
-- that return functor values. Remember how we used the applicative functors for having a system where we could call
-- functions over functor values just like normal values?
-- For f x y with normal values, we would have pure f <$> x <*> y with functor values.
-- With Monads, we can have functions that work with (Functor m) => a -> m b, and use them like:
-- let square x = Just (x * x); double x = Just (2 * x) in Just 3 >>= square >>= double or: in square 3 >>= double
-- as you can see square and double are functions that return functors, however we can apply them on Maybe a, just like
-- we would call them if they were to work with normal values: double $ square 3, just in reverse order.

-- Monads have some properties that makes us possible to work with them.
-- return :: (Monad m) => a -> m a
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b, a.k.a. bind, m >>= f is same as join (fmap f m)
-- (>>) :: (Monad m) => m a -> m b -> m b
-- fail :: String -> m a, with the default definition of fail msg = error msg

-- Monads have some rules as well as monoids, functors, applicative functors.
-- left identity: return x >>= f should be equal to f x
-- right identity: x >>= return should be equal to x
-- associativity: (x >>= f) >>= g should be equal to x >>= (\x -> f x >>= g)

-- Simple example shown on learn you a haskell, where given a game of balancing number of birds in sides of a pole,
-- in case the number of bird differences between sides of the pole is greater than 4, the game will be lost.
-- to implement this, we can use the fact that Functors keep their computational context, and go with a Maybe.
-- when any number of birds are added to a side of the rope, in case the difference is greater than 4, we return Nothing
-- and chain the calls after that.
type BirdCount = Int
type Pole = (BirdCount, BirdCount)
data PoleSide = L | R deriving (Show, Read)

isInvalid pole = let (left, right) = pole in (abs $ left - right) > 4
leftAdd, rightAdd :: BirdCount -> Pole -> Maybe Pole
leftAdd n (left, right)  = let newPole = (left + n, right) in if isInvalid newPole then Nothing else Just newPole
rightAdd n (left, right) = let newPole = (left, right + n) in if isInvalid newPole then Nothing else Just newPole

addBird :: Pole -> (PoleSide, BirdCount) -> Maybe Pole
addBird p (L, n) = leftAdd n p
addBird p (R, n) = rightAdd n p

addBirds :: Pole -> [(PoleSide, BirdCount)] -> Maybe Pole
addBirds pole steps = foldM addBird pole steps

birdAddMonadExample = return (5, 2) >>= rightAdd 3 >>= leftAdd 4 >>= rightAdd 8

-- >> is a function in the form of (Monad m) => m a -> m b -> m b
-- which essentially returns the second monad, in case the left monad is not empty(Nothing, [], Left etc.)
-- think of it calling (>>) x y = x >>= (\_ -> y)
ignoreResultPrintExample = putStr "Type " >> putStr "Something: " >> (\x -> "You typed: " ++ x) <$> getLine >>= putStrLn

-- remember for lists x and y, we had length x * length y elements for applicative functors [a -> b] <*> [a], this is
-- because with applicative functors, we could have multiple functions defined, with monads we work over arrays with a
-- single functions, so we will only have elements as the count of the array we are working on.
-- To do list comprehensions with Monads, the monad should implement the MonadPlus class where MonadPlus has
-- mzero :: m a | the empty/zero value to stop the operations for.
-- mplus :: m a -> m a -> m a | defined for concatenation of multiple monads into one, Nothing x becomes Nothing etc.
doWithListMonadExample = do
    -- list comprehensions are syntactic sugar for using lists as monads
    -- <- just gets the value out of a functor value and makes it normal(with pattern matching aswell) again.
    -- In case we work with Maybe, this function would return Nothing in case the functor value had Nothing in it.
    -- This depends on the fail implementation of monad.
    (c, r) <- [(x, y) | x <- [1..5], y <- [1..3], or [odd x, odd y]]
    -- you can also use guard from Control.Monad here. which has the type signature of: Bool -> f ()
    -- think of each of these do lines as one single continuous lambda function. each line has access to variables, defined
    -- before it. and each line you have is connected to the previous with >>= so if you were to have Nothing here,
    -- the function would resolve into Nothing.
    -- so we can also say:
    -- guard $ or [odd c, odd r]
    return [c, r]

doWithMaybeMonadExample :: Maybe (Int, Int) -> Maybe Int
doWithMaybeMonadExample m = do
    (x, y) <- m
    return (x + y)

-- the bind(>>=) definition for functions is:
-- h >>= f = \w -> f (h w) w
-- which makes the definition of f as: a -> b -> c
-- (+2) >>= (*) $ 5 == 35
doWithFunctionExample = do
    a <- (*2)
    b <- (+10)
    return (a + b)

-- or could be written like this as well:
lambdaMonadFunctionExample = (*2) >>= (\a -> (+10) >>= (\b -> return (a + b)))

type KnightPos = (Int, Int)

-- Relative moves of a knight, 8 possible move
knightMoves = [x | x <- mapM (const [1,-1,-2,2]) [1..2], (sum $ map abs x) == 3]

moveKnight :: KnightPos -> [KnightPos]
moveKnight start = do
    let (r, c) = start
    -- foreach relative position, add to the start...
    [nr, nc] <- zipWith (zipWith (+)) knightMoves $ repeat [r, c]
    -- Control.Monad guard to check if we are in the board...
    guard (nr `elem` [1..8] && nc `elem` [1..8])
    return (nr, nc)

-- Create a list of possible positions a knight can be in 3 steps, given a start position
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- Create a list of possible positions a knight can be, given a start position and number of steps
knightPosIn :: Int -> KnightPos -> [KnightPos]
-- using fold to go over a list with a binding function
knightPosIn n start = foldl (>>=) (return start) (replicate n moveKnight)

-- Check if a knight can reach a given position in 3 moves
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start dest = dest `elem` in3 start

-- Check if a knight can reach a given position in n moves
canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn n start dest = dest `elem` (knightPosIn n start)

-- this is essentially a >>= (\x -> b >>= g x)
-- that is why the second lambda can access the first variable.
lambdaScopeExample = Just 5 >>= \x -> Just 6 >>= \y -> return (x + y)

-- Some useful functions with Monads
-- liftM is the same with fmap however it works with Monad constraint, where fmap has a constraint on Functor
-- liftM is there because Monad's dont require to be Functors
liftMExample = (+5) `liftM` (Just 5)

-- ap can be defined easier with monads, using the do notation:
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
  f <- mf
  x <- m
  return $ f x

-- join :: (Monad m) => m (m a) -> m a
-- this function is >>= id
