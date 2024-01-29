
circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h


-- Question 1
timesTen :: Int -> Int
timesTen x = x * 10

-- Question 2
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- Question 3
areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^ 2

-- Question 4
volumnOfCylinder :: Float -> Float -> Float
volumnOfCylinder h r = areaOfCircle r * h

-- Question 5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt (y1 - y2) ^ 2 + (x1 - x2) ^ 2

-- Question 6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

-- Question 7 
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0
-- divisibleBy x y = mod x y == 0


-- Question 8 
isEven :: Int -> Bool
isEven x = divisibleBy x 2

-- Question 9 
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

-- Question 10
absolute :: Int -> Int
absolute x = if x < 0 then -x else x