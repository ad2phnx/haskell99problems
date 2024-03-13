module P11to20 where

import P1to10

data Encoded a = Single a | Multiple Int a
  deriving (Show)

-- Problem 11
encodeMod :: (Eq a) => [a] -> [Encoded a]
encodeMod [] = []
encodeMod lst = encodeMod' $ pack lst
 where
  encodeMod' :: [[a]] -> [Encoded a]
  encodeMod' [] = []
  encodeMod' (y : ys)
    | length y == 1 = Single (myFirst y) : encodeMod' ys
    | otherwise = Multiple (myLength y) (myFirst y) : encodeMod' ys

instance (Eq a) => Eq (Encoded a) where
  (Single x) == (Single y) = x == y
  (Multiple n1 x) == (Multiple n2 y) = n1 == n2 && x == y
  _ == _ = False

-- Problem 12
decodeMod :: [Encoded a] -> [a]
decodeMod [] = []
decodeMod (Single x : xs) = x : decodeMod xs
decodeMod (Multiple n1 x : xs) = replicate n1 x ++ decodeMod xs

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (x : xs) = encodeDirect' 1 x xs

encodeDirect' :: (Eq a) => Int -> a -> [a] -> [Encoded a]
encodeDirect' n x [] = [if n == 1 then Single x else Multiple n x]
encodeDirect' n x (y : ys)
  | x == y = encodeDirect' (n + 1) x ys
  | otherwise = (if n == 1 then Single x else Multiple n x) : encodeDirect' 1 y ys

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) n = replicate n x ++ repli xs n

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = [x | (index, x) <- zip [1 ..] xs, index `mod` n /= 0]
