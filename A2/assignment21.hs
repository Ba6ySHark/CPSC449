--Question 1
transposeMatrix :: [[a]] -> [[a]]
-- write your code here -- 
transposeMatrix matrix = case matrix of
    []      -> []
    []:_    -> []
    _       -> map head matrix : transposeMatrix (map tail matrix)

-- Question 2
groupByPredicate :: (a -> Bool) -> [a] -> [[a]]
groupByPredicate _ [] = []
groupByPredicate func (x:xs)
    | func x    = trueList [x] xs
    | otherwise = falseList [x] xs
  where
    trueList accumulated [] = [accumulated]
    trueList accumulated (y:ys)
        | func y    = trueList (accumulated ++ [y]) ys
        | otherwise = accumulated : groupByPredicate func (y:ys)

    falseList accumulated [] = [accumulated]
    falseList accumulated (y:ys)
        | not (func y) = falseList (accumulated ++ [y]) ys
        | otherwise    = accumulated : groupByPredicate func (y:ys)


-- --Question 3
-- data Movie = Movie { title :: String, rating :: Float, genre :: String } deriving Show

-- topRatedMovies :: [Movie] -> Float -> [String]
-- -- write your code for here --

-- moviesByGenre :: [Movie] -> String -> [String]
-- -- write your code here --

-- adjustedRatings :: [Movie] -> [Movie]
-- -- write your code here --


-- --Question 4
-- mergeSort :: Ord a => [a] -> [a]
-- -- write your code here --
