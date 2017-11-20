module Writer where
import Control.Monad.Writer

-- Be careful with your selection of monoid. For example Lists will use ++ for concatenating in mappend. So you can have
-- a writer that causes slow down each time you call tell, because it will add to end of the list.

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (result, newLog) = f x in (result, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

addDrinkExample = ("beans", Sum 30) `applyLog` addDrink

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 73
    b <- logNumber 42
    return (a + b)

multWithLogExample = let (r, msg) = runWriter multWithLog in (mapM_ putStrLn msg) >> (putStrLn $ "Result: " ++ show r)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with: " ++ show a]
        return a
    | otherwise = do
        -- if this tell were to be after the recursive call, each mappend would add the log to the end of the list
        -- making it slower, we could use something like difflist to prevent this slow down, or log in a way which
        -- we can append to the beginning of the list like we are doing right now.
        tell ["Current a: " ++ show a ++ ", b: " ++ show b ++ ", mod res: " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

gcdExample = runWriter $ gcd' 104 32