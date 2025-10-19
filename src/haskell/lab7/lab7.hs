
-- >>> secondToLast "abcd"
-- 'c'
secondToLast :: [a] -> a
secondToLast [x, y] = x
secondToLast (x:xs) = secondToLast xs

-- >>> myReverse "abcd"
-- "dcba"
myReverse :: [a] -> [a]
myReverse xs = helper [] xs
  where
    helper acc [] = acc
    helper acc (x:xs) = helper (x:acc) xs

-- >>> myFlatten [['a'], ['b', 'c'], ['d']]
-- "abcd"
myFlatten :: [[a]] -> [a]
myFlatten xs = helper [] xs
  where
    helper acc [] = acc
    helper acc ([]:xs) = helper acc xs
    helper acc ((y:ys):xs) = helper (acc ++ [y]) (ys:xs)

-- >>> myEncode "aaaabccaadeeee"
-- [('a',4),('b',1),('c',2),('a',2),('d',1),('e',4)]
myEncode :: (Eq a, Num b) => [a] -> [(a, b)]
myEncode (x:xs) = helper xs x 1 []
  where
    helper [] ch count result = result ++ [(ch, count)]
    helper (x:xs) ch count result
      | x == ch = helper xs ch (count + 1) result
      | otherwise = helper xs x 1 (result ++ [(ch, count)])

-- >>> myDecode [('a',4),('b',1),('c',2),('a',2),('d',1),('e',4)]
-- "aaaabccaadeeee"
myDecode :: [(Char, Int)] -> String
myDecode [] = []
myDecode ((ch, count):xs) = helper xs ch count []
  where
    helper [] _ 0 result = result
    helper ((ch, count):xs) _ 0 result = helper xs ch count result
    helper xs ch count result = helper xs ch (count - 1) (result ++ [ch])

sumOf :: (Double -> Double) -> [Double] -> Double
sumOf f = foldr ((+) . f) 0.0

-- >>> mySum [1, 2, 3]
-- 6.0
mySum = sumOf id

-- >>> sumOfSquares [1, 2, 3]
-- 14.0
sumOfSquares = sumOf (^ 2)

-- >>> length' [1, 2, 3]
-- 3.0
length' = sumOf (const 1)

type UserId = String
data Status = Sending | Sent | Read
data Message = Message UserId String Status

-- >>> lastMessageStatus "id1" [Message "id1" "Hello" Sent]
-- No instance for `Show Status' arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a1RZG
lastMessageStatus :: UserId -> [Message] -> Maybe Status
lastMessageStatus userId messages = helper userId messages Nothing
  where
    helper userId [] status = status
    helper userId (Message msgUserId _ msgStatus : xs) status
      | msgUserId == userId = helper userId xs (Just msgStatus)
      | otherwise = helper userId xs status

-- >>> mySplit 3 "abcdefghik"
-- ("abc","defghik")
mySplit :: Int -> String -> (String, String)
mySplit n xs = helper [] n xs
  where
    helper left 0 right = (left, right)
    helper left n (x:xs) = helper (left ++ [x]) (n - 1) xs

-- >>> chunks 3 "abcdefghik"
-- ["abc","def","ghi","k"]
chunks :: Int -> String -> [String]
chunks n xs = helper n xs [] []
  where
    helper :: Int -> String -> String -> [String] -> [String]
    helper count [] [] result = result
    helper count [] chunk result = result ++ [chunk]
    helper 0 xs chunk result = helper n xs [] (result ++ [chunk])
    helper count (x:xs) chunk result = helper (count - 1) xs (chunk ++ [x]) result

-- >>> choose 3 "abcde"
-- ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
choose :: Int -> String -> [String]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = withX ++ withoutX
  where
    withX = map (x:) (choose (n - 1) xs)
    withoutX = choose n xs


-- день месяца (от 1 до 31)
newtype Day = Day Int deriving (Show)
-- месяц года (от 1 до 12)
newtype Month = Month Int deriving (Show)
-- год
newtype Year = Year Int deriving (Show)
-- корректная дата
data Date = Date Day Month Year deriving (Show)

today :: Date
today = Date (Day 13) (Month 10) (Year 2025)

-- >>> yearOf today
-- Year 2025
yearOf :: Date -> Year
yearOf (Date _ _ y) = y

-- >>> monthOf today
-- Month 10
monthOf :: Date -> Month
monthOf (Date _ m _) = m

-- >>> dayOf today
-- Day 13
dayOf :: Date -> Day
dayOf (Date d _ _) = d

-- >>> prettyDate today
-- "13 oct 2025"
prettyDate :: Date -> String
prettyDate (Date (Day d) (Month m) (Year y)) =
    show d ++ " " ++ monthName ++ " " ++ show y
  where
    monthName = case m of
        1  -> "jan"
        2  -> "feb"
        3  -> "mar"
        4  -> "apr"
        5  -> "may"
        6  -> "jun"
        7  -> "jul"
        8  -> "aug"
        9  -> "sep"
        10 -> "oct"
        11 -> "nov"
        12 -> "dec"
        _  -> "unknown month"

-- >>> isLeapYear (Year 2000)
-- True
-- >>> isLeapYear (Year 1900)
-- False
-- >>> isLeapYear (Year 2024)
-- True
-- >>> isLeapYear (yearOf today)
-- False
isLeapYear :: Year -> Bool
isLeapYear (Year y) = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)


-- >>> daysInMonth (Year 2000) (Month 2)
-- 29
-- >>> daysInMonth (Year 2001) (Month 2)
-- 28
-- >>> daysInMonth (Year 2025) (Month 10)
-- 31
daysInMonth :: Year -> Month -> Int
daysInMonth year (Month m) = case m of
    1  -> 31 
    2  -> if isLeapYear year then 29 else 28 -- Февраль
    3  -> 31 
    4  -> 30 
    5  -> 31 
    6  -> 30 
    7  -> 31 
    8  -> 31 
    9  -> 30 
    10 -> 31 
    11 -> 30 
    12 -> 31 
    _  -> 0

normalizeDate :: Date -> Date
normalizeDate (Date (Day d) (Month m) (Year y))
    | m > 12 = normalizeDate (Date (Day d) (Month (m - 12)) (Year (y + 1)))
    | d > daysInMonth (Year y) (Month m) =
        let daysInCurrentMonth = daysInMonth (Year y) (Month m)
        in normalizeDate (Date (Day (d - daysInCurrentMonth)) (Month (m + 1)) (Year y))
    | otherwise = Date (Day d) (Month m) (Year y)

-- >>> prettyDate (addYears 1 today)
-- "13 oct 2026"
addYears :: Int -> Date -> Date
addYears n (Date d m (Year y)) = Date d m (Year (y + n))

-- >>> prettyDate (addMonths 3 today)
-- "13 jan 2026"
addMonths :: Int -> Date -> Date
addMonths n (Date d (Month m) y) = normalizeDate (Date d (Month (m + n)) y)

-- >>> prettyDate (addDays 100 today)
-- "21 jan 2026"
addDays :: Int -> Date -> Date
addDays n (Date (Day d) m y) = normalizeDate (Date (Day (d + n)) m y)
