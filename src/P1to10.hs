module P1to10 where

data NestedList a = Elem a | List [NestedList a]

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_ : xs) = myLast xs

myFirst :: [a] -> a
myFirst [] = error "Empty List"
myFirst [x] = x
myFirst (x : _) = x

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [_] = error "Only 1 element"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty List"
elementAt (x : xs) n
    | n < 1 || n > (length xs + 1) = error "Out of bounds"
    | n > 1 = elementAt xs (n - 1)
    | otherwise = x

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs = xs == myReverse xs

-- Probem 7
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = foldr (++) [] $ map flatten' xs

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : xs) = if x == myFirst xs then compress xs else x : compress xs

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x : xs) = pack' xs [x]
  where
    pack' :: (Eq a) => [a] -> [a] -> [[a]]
    pack' [] current = [current]
    pack' (y : ys) current@(z : _)
        | y == z = pack' ys (y : current)
        | otherwise = current : pack' ys [y]

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode lst = encode' $ pack lst
  where
    encode' :: [[a]] -> [(Int, a)]
    encode' [y] = [(myLength y, myFirst y)]
    encode' (y : ys) = (myLength y, myFirst y) : encode' ys
