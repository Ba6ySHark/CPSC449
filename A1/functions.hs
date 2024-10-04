import Data.Char
import Language.Haskell.TH (safe, nameBase)
import qualified Data.ByteString.Lazy.Char8 as Q4

-- Q1
multiply :: Int -> Int -> Int
multiply x y
    | x == 0 || y ==0   = 0
    | y > 0             = x + multiply x (y-1)
    | y < 0             = negate (multiply x (-y))

-- Q2
binomial :: Int -> Int -> Int
binomial n k
    | k > n             = 0
    | k == 0 || k == n  = 1
    | otherwise         = div ((n-k+1) * binomial n (k-1)) k

-- Q3.a
myShow :: Int -> String
myShow n
    | n < 10        = [chr (n + 48)]
    | otherwise     = myShow (quot n 10) ++ [chr (rem n 10 + 48)]

-- Q3.b
myLength :: String -> Int
myLength [] = 0
myLength (_:sub_string) = myLength sub_string + 1

myEven :: Int -> Bool
myEven n
    | n == 0        = True
    | n == 1        = False
    | otherwise     = myEven (n - 2)

processAndCheck :: Int -> Bool
processAndCheck = myEven . myLength . myShow

-- Q4.a
powerOfTwoMaybe :: Integer -> Maybe Integer
powerOfTwoMaybe n
    | n >= 0         = Just (2 ^ n)
    | otherwise     = Nothing

-- Q4.b
data SF a = SS a | FF

factorialSF :: Integer -> SF Integer
factorialSF n
    | n < 0         = FF
    | otherwise     = SS (fact n)
  where
    fact 0 = 1
    fact n = n * fact (n - 1)

-- Q4.c
invExpEither :: Integer -> Either String Integer
invExpEither n
    | n < 0     = Left "Input must be a non-negative integer"
    | otherwise = Right (largestExp n 0)
  where
    largestExp x exp
      | 2^exp > x  = exp - 1
      | otherwise  = largestExp x (exp + 1)