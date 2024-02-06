

-- Question 1
absolute :: Int -> Int
absolute x 
    | x < 0 = -x
    | otherwise = x

-- Question 2
sign :: Int -> Int
sign x
    | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1

-- Question 3
--howManyEqual :: Int -> Int -> Int -> Int
--howManyEqual x y z 
--    | 


-- Question 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagonalLength x + diagonalLength y + diagonalLength z
    where 
        diagonalLength x = sqrt (2 * x^2) 

-- Question 5
taxiFare :: Int -> Float
taxiFare dist = 2.20 + first10 + after10
    where 
        first10
            | dist >= 10 = 10.0 * 0.5
            | dist < 10 = fromIntegral dist * 0.5
            | otherwise = 0
        after10 
            | dist > 10 = fromIntegral (dist - 10) * 0.3
            | otherwise = 0 


-- Question 6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = 0 

-- Question 7
validDate :: Int -> Int -> Bool
validDate day month 
    | day < 1 = False
    |   month == 1 || month == 3 || 
        month == 5 || month == 7 || 
        month == 8 || month == 10 || 
        month == 12 
        && day <= 31 = True

    |   month == 4 || month == 6 ||
        month == 9 || month == 11 
        && day <= 30 = True
    
    | month == 2 && day <= 28 = True
    | otherwise = False

-- Question 8 
daysInMonth :: Int -> Int -> Int
daysInMonth month year 
    | month == 1 = 31
    | month == 2 && isLeapYear = 29
    | month == 2 = 28
    | month == 3 = 31
    | month == 4 = 30
    | month == 5 = 31
    | month == 6 = 30
    | month == 7 = 31
    | month == 8 = 31
    | month == 9 = 30
    | month == 10 = 31
    | month == 11 = 30
    | month == 12 = 31
    | otherwise = 0 
    where isLeapYear 
            | year `mod` 4 == 0 = True
            | otherwise = False