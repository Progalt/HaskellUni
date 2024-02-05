

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
howManyAboveAverage x y z 
    | x > avg && y > avg && z > avg = 3
    where 
        avg = fromIntegral (x + y + z) / 3