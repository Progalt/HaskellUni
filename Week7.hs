
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)  
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type 
data Shape = Circle Float |
             Rectangle Float Float
     deriving(Show)

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String | 
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null | 
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

-- Question 1
data Month = January | February | March | April | May | June | July | August | September | October | November | December
               deriving (Eq,Ord,Show,Read)

data Season = Winter | Spring | Summer | Autumn
               deriving (Eq,Ord,Show,Read)


-- Question 2
season :: Month -> Season
season m
    | m <= February = Winter
    | m <= May = Spring 
    | m <= August = Summer
    | m <= November = Autumn
    | otherwise = Winter    


-- Question 3
numberOfDays :: Month -> Int -> Int
numberOfDays month year
     | month == February && isLeapYear year = 29
     | month == February = 28
     | month == April = 30
     | month == June = 30
     | month == September = 30
     | month == November = 30
     | otherwise = 31
     where 
          isLeapYear :: Int -> Bool
          isLeapYear year = year `mod` 4 == 0


-- Question 4 

data Point = Point Float Float
     deriving(Show)

-- Question 5

data PositionedShape = PositionedShape  Shape Point 
     deriving(Show)

-- Question 6
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point x y)) nx ny = (PositionedShape shape (Point nx ny))

-- Question 7 
numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node value left right) = 1 + numberOfNodes left + numberOfNodes right

-- Question 8
isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember member (Node value left right)
    | value == member = True
    | otherwise = isMember member left || isMember member right

-- Question 9 
leaves :: Tree -> [Int]
leaves Null = []
leaves (Node value Null Null) = [value]
leaves (Node value left right) = leaves left ++ leaves right

-- Question 10
inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node value left right) = inOrder left ++ [value] ++ inOrder right

-- Question 11
insert :: Int -> Tree -> Tree
insert value Null = Node value Null Null
insert value (Node nodeValue left right)
     | value < nodeValue = Node nodeValue (insert value left) right
     | otherwise = Node nodeValue left (insert value right)

-- Question 12
listToSearchTree :: [Int] -> Tree
listToSearchTree list = intoTree list Null
     where 
          intoTree :: [Int] -> Tree -> Tree
          intoTree [] tree = tree
          intoTree (x : list) tree = intoTree list (insert x tree)

binaryTreeSort :: [Int] -> [Int]
binaryTreeSort list = inOrder (listToSearchTree list)