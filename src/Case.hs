module Case where

getLength' [] = 0
getLength' (x:xs) = 1 + (getLength' xs)

-- pattern matching in functions are syntatic sugars for case expressions.
getLengthCase' xs = case xs of [] -> 0; (x:xs) -> 1 + (getLengthCase' xs)

-- shows the possible length of the given array.
showPossibleLength' xs = "The list length is " ++ (case xs of [] -> "zero"; (x: []) -> "one"; otherwise -> "over one") ++ "."

-- same with above but using syntatic sugar of function definitions inside where.
showPossibleLengthWhere' xs = "The list length is " ++ what xs ++ "."
    where what [] = "zero"; what (x:[]) = "one"; what (x: xs) = "over one"

-- same with above but with let expression instead of where
showPossibleLengthLet' xs =
    let
        what [] = "zero"
        what (x:[]) = "one"
        what (x: xs) = "over one"
    in "The list length is " ++ what xs ++ "."

-- same with above but one line
showPossibleLengthLetOneLiner' xs = let what [] = "zero"; what (x:[]) = "one"; what (x: xs) = "over one" in "The list length is " ++ what xs ++ "."
