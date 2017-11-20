module Stack where

newtype State s a = State { runState :: s -> (a, s) }

type Stack = [Int]

instance Functor (State s) where
    fmap f (State g) =
        State $ \s -> let (value, newState) = g s in (f value, newState)

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State h) <*> (State f) =
        State $ \s -> let (converter, newState) = h s
                          (toConvert, resState) = f newState
                      in (converter toConvert, resState)

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState

getState :: State Stack Stack
getState = State $ \xs -> (xs, xs)

putState :: Stack -> State Stack ()
putState state = State $ \_ -> ((), state)

push :: Int -> State Stack ()
push x = State $ \xs -> ((), x:xs)

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

customStateExample = runState (pop) [1,2,3]

customStateDo = do
  state <- getState
  push ((state !! 2) + 5)
  -- ignore the previous states held, store a new one
  putState [5]
  -- using the flatmap syntax instead of using <-
  getState >>= (\state -> push ((state !! 0) + 5))

customStateWithDoExample = runState customStateDo [1,2,3]
