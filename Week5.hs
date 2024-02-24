{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []


-- Question 1
headPlusOne :: [Int] -> Int
headPlusOne [] = -1 
headPlusOne lst = head lst + 1

-- Question 2
duplicateHead :: [a] -> [a] 
duplicateHead [] = []  
duplicateHead lst =  head lst : lst

-- Question 3
rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x:y:lst) = y : x : lst

-- Question 4
listLength :: [a] -> Int
listLength [] = 0
listLength (_ : lst) = 1 + listLength lst

-- Question 5
multAll :: [Int] -> Int
multAll [] = 1
multAll (x:lst) = x * multAll lst

-- Question 6
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:lst) = x && andAll lst

-- Question 7
orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:lst) = orAll lst || x

-- Question 8
countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers num (x:lst)
    | x == num = 1 + countIntegers num lst
    | otherwise = countIntegers num lst

-- Question 9
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll num (x:lst)
    | x == num = removeAll num lst
    | otherwise = x : removeAll num lst

-- Question 10
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst num (x:lst)
    | x == num = x : removeAll num lst
    | otherwise = x : removeAllButFirst num lst

type StudentMark = (String, Int)
testData :: [StudentMark]
testData =
    [   ("John", 53),
        ("Sam", 16),
        ("Kate", 85),
        ("Jill", 65),
        ("Bill", 37),
        ("Amy", 22),
        ("Jack", 41),
        ("Sue", 71)
    ]


-- Question 11
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks "" _ = []
listMarks name ((n, m) : lst)
    | name == n = m : listMarks name lst
    | otherwise = listMarks name lst

-- Question 12
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:lst) = x <= y && sorted(y : lst)

-- Question 13
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xlst) (y:ylst) = x == y && prefix xlst ylst

-- Question 14
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence xlst ylst
    | prefix xlst ylst = True
    | otherwise = subSequence xlst (tail ylst)