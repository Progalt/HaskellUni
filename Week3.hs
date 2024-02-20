-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&), gcd)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m


-- Question 1

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False

testAnd :: Bool
-- testAnd = True && True
-- testAnd = False && True
-- testAnd = True && False
testAnd = False && False

-- Question 2

exOr :: Bool -> Bool -> Bool
exOr False x = x
exOr True x = not x

-- Question 3

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x
ifThenElse _ _ y = y 

-- Question 4

daysInMonth :: Int -> Int
daysInMonth 2 = 28 
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30 
daysInMonth 11 = 30
daysInMonth _ = 31

validDate :: Int -> Int -> Bool
validDate day month = day <= daysInMonth month

-- Rewritten for Question 11
{-
-- Question 5

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers x = x + sumNumbers (x - 1)
-- Question 6

sumSquares :: Int -> Int
sumSquares 0 = 0 
sumSquares x = x * x + sumSquares (x - 1)

-}

-- Question 7 
power :: Int -> Int -> Int
power _ 0 = 1
power num pow = num * power num (pow - 1)

-- Question 8 

sumFromTo :: Int -> Int -> Int
sumFromTo from to 
  | to < from = 0
  | from == to = from
  | otherwise = from + sumFromTo (from + 1) to

-- Question 9

gcd :: Int -> Int -> Int
gcd x y
  | x == y = x
  | x < y = gcd (y - x) x
  | otherwise = gcd (x - y) y

-- Question 10

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot x y
  | y * y <= x = y
  | otherwise = findRoot x (y - 1)

-- Question 11

sumNumbers :: Int -> Int
sumNumbers x
    | x > 0 = x + sumNumbers (x - 1)
    | otherwise = x

sumSquares :: Int -> Int
sumSquares x
    | x > 0 =  x * x + sumSquares (x - 1)
    | otherwise = x 

