module FizzBuzz where

getFizz x = if x `mod` 3 == 0 then "Fizz" else ""
getBuzz x = if x `mod` 5 == 0 then "Buzz" else ""

getFizzBuzzSimple x = if length (getFizz x ++ getBuzz x) == 0 then show x else getFizz x ++ getBuzz x

getFizzBuzzGuard x
    | null calcd = show x
    | otherwise  = calcd
    where calcd = getFizz x ++ getBuzz x
