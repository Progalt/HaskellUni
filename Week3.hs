-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&))

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

(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False

infixr 3 &&

testAnd :: Bool
-- testAnd = True && True
-- testAnd = False && True
-- testAnd = True && False
testAnd = False && False

-- Question 2

exOr :: Bool -> Bool -> Bool
exOr False False = False
exOr False True = True
exOr True False = True
exOr True True = False

-- Question 3

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x y = x
ifThenElse False x y = y 

-- Question 4

daysInMonth :: Int -> Int
daysInMonth 2 = 28 
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30 
daysInMonth 11 = 30
daysInMonth n = 31

validDate :: Int -> Int -> Bool
validDate day month = day <= daysInMonth month

-- Question 5

sumNumbers :: Int -> Int
sumNumbers x
    | x > 0 = x + sumNumbers (x - 1)
    | otherwise = x

-- Question 6

sumSquares :: Int -> Int
sumSquares x
    | x > 0 =  x * x + sumSquares (x - 1)
    | otherwise = x 

-- Question 7 
