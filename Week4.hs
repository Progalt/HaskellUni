import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)



-- Question 1 
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

-- Question 2
grade :: StudentMark -> Char
grade (name, mark) 
    | mark < 0 || mark > 100 = error "Mark outside of valid range"
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

-- Question 3
capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark < 0 || mark > 100 = error "Mark outside of valid range"
    | mark > 40 = (name, 40)
    | otherwise = (name, mark)

-- Question 4
firstNumbers :: Int -> [Int]
firstNumbers x = [1 .. x]

-- Question 5
firstSquares :: Int -> [Int]
firstSquares x = [y^2 | y <- [1 .. x]]

-- Question 6 
capitalise :: String -> String
capitalise str = [toUpper y | y <- str]

-- Question 7 
onlyDigits :: String -> String
onlyDigits str = [y | y <- str, isDigit y]

-- Question 8 
capMarks :: [StudentMark] -> [StudentMark] 
capMarks marks = [capMark y | y <- marks]

-- Question 9 
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents marks = [(name, grade (name, mark)) | (name, mark) <- marks]

-- Question 10
duplicate :: String -> Int -> String
duplicate str x
    | x > 0 = str ++ duplicate str (x - 1)
    | otherwise = ""

duplicate' :: String -> Int -> String
duplicate' str x = concat [str | _ <- [1 .. x]]

-- Question 11
divisors :: Int -> [Int]
divisors x = [y | y <- [1 .. x], mod x y == 0]

-- Question 12
-- A prime number is divisible by 1 and itself
-- so the length of the array from divisors is 2
isPrime :: Int -> Bool
isPrime x = length ( divisors x ) == 2

-- Question 13
split :: [(a,b)] -> ([a],[b])