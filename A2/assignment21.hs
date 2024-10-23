--Question 1
transposeMatrix :: [[a]] -> [[a]]
-- write your code here -- 
transposeMatrix matrix = case matrix of
    []      -> []
    []:_    -> []
    _       -> map head matrix : transposeMatrix (map tail matrix)

-- --Question 2
-- groupByPredicate :: (a -> Bool) -> [a] -> [[a]]
-- -- write your code here --


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
