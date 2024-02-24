{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

andAll :: [Bool] -> Bool
andAll xs = foldr (&&) True xs


-- Question 1
mult10 :: [Int] -> [Int]
mult10 = map (* 10)

-- Question 2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

-- Question 3
orAll :: [Bool] -> Bool
orAll = foldr (||) False

-- Question 4
sumSquares :: [Int] -> Int
sumSquares = sum . map (^2) 

-- Question 5
-- TODO: Rewrite without lambda
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (\x -> x >= 0 && x <= 10)

-- Question 6
squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>= 0)

-- Question 7
countBetween :: Float -> Float -> [Float] -> Int
countBetween mn mx lst = length ( filter (\x -> x >= mn && x <= mx) lst )

-- Question 8
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f lst = length ( filter (<0) ( map f lst )) <= 1
-- alwaysPositive f lst = andAll (map (\x -> x > 0) (map f lst))
alwaysPositive f = andAll . map ((>0) .  f)

-- Question 9
productSquareRoots :: [Float] -> Float
-- productSquareRoots = foldr (*) 1 . map sqrt . filter (>0)
productSquareRoots = foldr ((*) . sqrt) 1 . filter (> 0)

-- Question 10
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f (x:lst) 
    | f x = lst
    | otherwise = x : removeFirst f lst

-- Question 11
-- removeLast :: (a -> Bool) -> [a] -> [a]
-- removeLast f (x:lst) 
--     | f (last lst) = x : init lst
--     | otherwise = x : removeLast f lst


-- Question 12
zeroToTen' :: [Int] -> [Int]
zeroToTen' = filter (\x -> x >= 0 && x <= 10)

-- Question 13
alwaysPositive' :: (Float -> Float) -> [Float] -> Bool
alwaysPositive' f = foldr (\x acc -> f x >= 0 && acc) True

productSquareRoots' :: [Float] -> Float
productSquareRoots' = foldr (\x acc -> if x > 0 then sqrt x * acc else acc) 1

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []