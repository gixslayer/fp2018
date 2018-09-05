module Database
where

type Person  =  (Name, Age, FavouriteCourse)

type Name             =  String
type Age              =  Integer
type FavouriteCourse  =  String

frits, peter, ralf, henk :: Person
frits  =  ("Frits",  33,  "Algorithms and Data Structures")
peter  =  ("Peter",  57,  "Imperative Programming")
ralf   =  ("Ralf",   33,  "Functional Programming")
henk   =  ("Henk",   90,  "Security")
john   =  ("John",   24,  "Functional Programming")

students   ::  [Person]
students   =  [frits, peter, ralf, henk, john]

age :: Person -> Age
age (_n, a, _c)  =  a

name :: Person -> Name
name (n, _, _) = n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, c) = c

showPerson :: Person -> String
showPerson (n, a, c) = "name=" ++ n ++ " age=" ++ show a ++ " favouriteCourse=" ++ c

twins :: Person -> Person -> Bool
twins (_, a1, _) (_, a2, _) = a1 == a2

increaseAge :: Person -> Person
increaseAge (n, a, c) = (n, a+1, c)

ex_6a :: [Person] -> [Person]
ex_6a = map (increaseAge . increaseAge)

ex_6b :: [Person] -> [Person]
ex_6b = map (\(n, a, c) -> (n ++ " dr", a, c))

ex_6c :: [Person] -> [Person]
ex_6c = filter (\p -> name p == "Frits")

ex_6d :: [Person] -> [Person]
ex_6d = filter (\p -> favouriteCourse p == "Functional Programming")

ex_6e :: [Person] -> [Person]
ex_6e = filter (\p -> age p >= 20 && age p < 30)

ex_6f :: [Person] -> [Person]
ex_6f = ex_6d . ex_6e

ex_6g :: [Person] -> [Person]
ex_6g = ex_6e . filter (\p -> favouriteCourse p == "Imperative Programming")

-- 1.5
thisOldMan :: String
thisOldMan = genThisOldMan 0

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

plays :: [String]
plays = ["on my thumb", "on my shoe", "on my knee", "on my door", "on my hive", "on my sticks", "up in heaven", "on my gate", "on my spine", "once again"]

genThisOldMan :: Int -> String
genThisOldMan n
    | n < length numbers = "This old man, he played " ++ numbers !! n ++ ",\nHe played knick-knack " ++ plays !! n ++ ";\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\n" ++ genThisOldMan (n+1)
    | otherwise = ""