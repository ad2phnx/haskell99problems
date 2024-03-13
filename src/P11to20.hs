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

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = dropEvery'' xs n 1 []

dropEvery'' :: [a] -> Int -> Int -> [a] -> [a]
dropEvery'' [] _ _ acc = acc
dropEvery'' (x : xs) n index acc
  | index `mod` n == 0 = dropEvery'' xs n (index + 1) acc
  | otherwise = dropEvery'' xs n (index + 1) (acc ++ [x])

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = split' xs n [] []

split' :: [a] -> Int -> [a] -> [a] -> ([a], [a])
split' [] _ acc1 acc2 = (reverse acc1, reverse acc2)
split' (x : xs) n acc1 acc2
  | n > 0 && n < length xs = split' xs (n - 1) (x : acc1) acc2
  | otherwise = split' xs n acc1 (x : acc2)

split1 :: [a] -> Int -> ([a], [a])
split1 [] _ = ([], [])
split1 xs n = split1' xs n []

split1' :: [a] -> Int -> [a] -> ([a], [a])
split1' xs 0 acc = (reverse acc, xs)
split1' (x : xs) n acc = split1' xs (n - 1) (x : acc)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i k = slice' xs (i - 1) k []

slice' :: [a] -> Int -> Int -> [a] -> [a]
slice' [] _ _ acc = reverse acc
slice' xs 0 0 acc = reverse acc
slice' (x : xs) i k acc
  | i > 0 = slice' xs (i - 1) (k - 1) acc
  | k > 0 = slice' xs 0 (k - 1) (x : acc)

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n >= 0 = drop n xs ++ take n xs
  | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = (undefined, [])
removeAt n xs = removeAt' (n - 1) xs []

removeAt' :: Int -> [a] -> [a] -> (a, [a])
removeAt' n (y : ys) acc
  | n == 0 = (y, acc ++ ys)
  | n > 0 = removeAt' (n - 1) ys (acc ++ [y])

removeAt1 :: Int -> [a] -> (a, [a])
removeAt1 _ [] = (undefined, [])
removeAt1 n xs
  | n <= 0 || n > length xs = error "Out of bounds"
  | otherwise = removeAt1' n xs []

removeAt1' :: Int -> [a] -> [a] -> (a, [a])
removeAt1' 1 (x : xs) acc = (x, reverse acc ++ xs)
removeAt1' n (y : ys) acc = removeAt1' (n - 1) ys (y : acc)
