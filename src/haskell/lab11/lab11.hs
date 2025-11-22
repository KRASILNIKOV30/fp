import Data.List (foldl')

-- >>> sizeGreaterThan 1 [1, 2, 3]
-- True
-- >>> sizeGreaterThan 3 [1, 2, 3]
-- False
-- >>> sizeGreaterThan 3 [1..]
-- True
sizeGreaterThan :: Int -> [a] -> Bool
sizeGreaterThan 0 [] = False
sizeGreaterThan 0 _ = True
sizeGreaterThan _ [] = False
sizeGreaterThan n (x : xs) = sizeGreaterThan (n - 1) xs

contains :: (Eq t) => [t] -> t -> Bool
contains [] a = False
contains (x : xs) a = x == a || xs `contains` a

-- >>> [1, 2, 3] `containsAllOf` [1, 3]
-- True
-- >>> [1, 3] `containsAllOf` [1, 2, 3]
-- False
-- >>> [1..] `containsAllOf` [1, 3 .. 9]
-- True
containsAllOf :: (Eq a) => [a] -> [a] -> Bool
containsAllOf _ [] = True
containsAllOf [] _ = False
containsAllOf orig (x : xs) = orig `contains` x && orig `containsAllOf` xs

-- >>> insert 7 [2, 4, 9]
-- [2,4,7,9]
-- >>> insert 7 [2, 4, 4]
-- [2,4,4,7]
-- >>> take 5 (insert 7 [2, 4 ..])
-- [2,4,6,7,8]
insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x : xs)
  | a < x = a : x : xs
  | otherwise = x : insert a xs

-- >>> separateBy ", " "hello"
-- "h, e, l, l, o"
-- >>> take 7 (separateBy [0] [1..])
-- [1,0,2,0,3,0,4]
separateBy :: [a] -> [a] -> [a]
separateBy _ [] = []
separateBy _ [x] = [x]
separateBy del (x : xs) = x : del ++ separateBy del xs

-- >>> goodLength "Привет, мир!"
-- 12
-- >>> goodLength [1..10^7]
-- 10000000
goodLength :: [a] -> Int
goodLength = foldl' (\a _ -> a + 1) 0

-- >>> noDuplicates "турыс!"
-- True
-- >>> noDuplicates [1, 3, 2, 5, 4, 6, 3, 0]
-- False
-- >>> noDuplicates ([1..5] ++ [1..])
-- False
noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates xs = noDups [] xs
  where
    noDups _ [] = True
    noDups seen (x : xs)
      | seen `contains` x = False
      | otherwise = noDups (x : seen) xs
