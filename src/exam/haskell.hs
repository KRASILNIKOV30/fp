data Parse a = Failed String | Parsed a deriving (Show)
data NonEmpty a = NonEmpty a [a] deriving (Show)

pInt :: String -> Parse Int
pInt str = readMaybe

>>> pSomeInts ["123", "345", "0"]
Parsed (NonEmpty 123 [345, 0])