-- Question 1.a
processNestedList :: [[Int]] -> [Int] 
processNestedList = map (^2) . filter even . concat

-- Question 1.b
fst' :: (a, b) -> a
fst' = uncurry (\x _ -> x)

snd' :: (a, b) -> b
snd' = uncurry (\_ y -> y)

-- Question 2.a
matrixAddition :: [[Int]] -> [[Int]] -> [[Int]]
matrixAddition = zipWith (zipWith (+))

-- Question 2.b
scalarMultiply :: Int -> [[Int]] -> [[Int]]
scalarMultiply scalar = map (map (* scalar))
-- Partial application example:
-- let doubleMatrix = scalarMultiply 2
-- doubleMatrix [[1, 2], [3, 4]]

-- Question 2.c
matrixTrace :: [[Int]] -> Int
matrixTrace matrix = foldr (+) 0 (zipWith (!!) matrix [0..])

-- Question 3
class Shape a where
    area :: a -> Double

data Circle = Circle Double
data Rectangle = Rectangle Double Double

instance Shape Circle where
    area (Circle r) = pi * r^2

instance Shape Rectangle where
    area (Rectangle l w) = l * w

-- -- Question 4
-- data AVLTree a = Empty | Node a (AVLTree a) (AVLTree a)

-- heightAVL :: AVLTree a -> Int
-- -- write your code here --

-- isOrdered :: (Ord a) => AVLTree a -> Bool
-- -- write your code here --

-- isBalanced :: AVLTree a -> Bool
-- -- write your code here --
