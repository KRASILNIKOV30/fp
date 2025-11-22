from :: Int -> [Int]
from n = n : from (n + 1)

-- >>> fromTo 2 5
-- [2,3,4,5]
-- >>> fromTo 0 0
-- [0]
-- >>> fromTo 3 2
-- []
fromTo :: Int -> Int -> [Int]
fromTo a b
    | a > b = []
    | otherwise = take (b - a + 1) (from a)
