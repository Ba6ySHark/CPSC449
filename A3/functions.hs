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

-- Question 4
data AVLTree a = Empty | Node a (AVLTree a) (AVLTree a)

-- Question 4.a
heightAVL :: AVLTree a -> Int
heightAVL Empty = -1
heightAVL (Node _ left right) = (max (heightAVL left) (heightAVL right)) + 1

-- Question 4.b
isOrdered :: (Ord a) => AVLTree a -> Bool
isOrdered Empty = True
isOrdered (Node value left right) =
  (allNodes (< value) left) &&
  (allNodes (> value) right) &&
  isOrdered left &&
  isOrdered right
  where
    allNodes :: (a -> Bool) -> AVLTree a -> Bool
    allNodes _ Empty = True
    allNodes condition (Node v l r) = 
      condition v && allNodes condition l && allNodes condition r

-- Question 4.c
isBalanced :: AVLTree a -> Bool
isBalanced Empty = True
isBalanced (Node _ left right) =
  abs (heightAVL left - heightAVL right) <= 1 &&
  isBalanced left &&
  isBalanced right

-- For testing in ghci (unbalalanced tree)
-- let tree = Node 10 (Node 5 (Node 2 Empty Empty) Empty) (Node 15 Empty Empty)
-- heightAVL tree // (expected 2)
-- isOrdered tree // (expected true)
-- isBalanced tree // (expected false)