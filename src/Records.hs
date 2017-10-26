module Records where
import qualified Data.Map as Map
import Data.Char

data Person = Person {
    firstName :: String,
    lastName :: String
} deriving (Show, Read, Eq, Ord)

upperCaseChar c
    | value >= 0 && value < 26 = chr $ ord 'A' + value
    | otherwise = c
    where value = ord c - ord 'a'
upperCaseStr str = map upperCaseChar str

exampleYigit = Person "Yiğitcan" "UÇUM"
exampleYigitNamed = Person { lastName = "UÇUM", firstName = "Yiğitcan" }
fullName (Person first last) = first ++ " " ++ last
fullNameNamed (Person { firstName = first, lastName = last}) = first ++ " " ++ last

upperCasedFullName person = (upperCaseStr . fullName) person
upperCasedFullNameWithDataChar person = map toUpper $ fullName person

-- data Maybe a = Nothing | Just a
-- Nothing equals to `Maybe a` which means it has a polymorphic type. Making it possible to use it anywhere that
-- requires a parameter of type Maybe. f.g. Maybe Int, Maybe [Char]. That is also how empty list is defined. it is [a]

data Vector a = Vector a a a deriving (Show)
vectorAdd :: (Num t) => Vector t -> Vector t -> Vector t
vectorAdd (Vector a b c) (Vector x y z) = Vector (a + x) (b + y) (c + z)

-- Enums and Bounded types
data Day = Pazartesi | Sali | Carsamba | Persembe | Cuma | Cumartesi | Pazar deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type synonyms
type Name = String; type Phone = String
type PhoneBook = [(Name, Phone)]
-- Below line is redundant. The type of the functions maybe infered by the haskell type system itself.
inPhoneBook :: Name -> Phone -> PhoneBook -> Bool
inPhoneBook pName pNumber pBook = (pName, pNumber) `elem` pBook

-- Parametrized type synonyms
-- type AssocList k v = [(k, v)]
-- type constructors can be partially applied just like functions.
-- type IntMap = Map Int
-- type constructors and value constructors are not the same things. Vector Int Int Int is a data constructor that takes
-- three integers and returns a value in the type of Vector Int.
data IkisindenBiri a b = Sol a | Sag b deriving (Show)
data Belki a = Bazi a | Hic deriving (Show)

matchEither :: IkisindenBiri a b -> Belki Bool
matchEither (Sol a) = Bazi True
matchEither _ = Hic

-- Usage example of IkisindenBiri and Belki with a Locker example where there are lockers identified with an integer
-- and can be occupied or not. In case of occupation or locker not existing, we get an error, otherwise we get the code
-- for it.
data LockerState = Free | Used deriving (Show)
data LockerError = NotExists | InUse deriving (Show)
type Code = String
type LockerRoom = Map.Map Int (LockerState, Code)

getCodeForLocker :: Int -> LockerRoom -> IkisindenBiri LockerError Code
getCodeForLocker lockerNumber lockerRoom = case Map.lookup lockerNumber lockerRoom of
    Nothing -> Sol NotExists
    Just (state, code) -> case state of
        Free -> Sag code
        Used -> Sol InUse
