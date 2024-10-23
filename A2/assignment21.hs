--Question 1
transposeMatrix :: [[a]] -> [[a]]
-- write your code here -- 
transposeMatrix matrix = case matrix of
    []      -> []
    []:_    -> []
    _       -> map head matrix : transposeMatrix (map tail matrix)

-- Question 2
groupByPredicate :: (a -> Bool) -> [a] -> [[a]]
-- write your code here -- 
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


--Question 3
data Movie = Movie { title :: String, rating :: Float, genre :: String } deriving Show

movies :: [Movie]
movies = [
    Movie "Inception" 8.8 "Sci-Fi",
    Movie "The Matrix" 8.7 "Sci-Fi",
    Movie "Jaws" 7.9 "Thriller",
    Movie "Frozen" 7.4 "Animation",
    Movie "Interstellar" 8.6 "Sci-Fi",
    Movie "The Godfather" 9.7 "Crime",
    Movie "Toy Story" 8.3 "Animation"
    ]

topRatedMovies :: [Movie] -> Float -> [String]
-- write your code for here --
topRatedMovies movies threshold = map title (filter (\movie -> rating movie > threshold) movies)

moviesByGenre :: [Movie] -> String -> [String]
-- write your code here --
moviesByGenre movies selected_genre = map title (filter (\movie -> genre movie == selected_genre) movies)

-- adjustedRatings :: [Movie] -> [Movie]
-- -- write your code here --


-- --Question 4
-- mergeSort :: Ord a => [a] -> [a]
-- -- write your code here --
