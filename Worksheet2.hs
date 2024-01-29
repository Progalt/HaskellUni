

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



