import Text.Read (readMaybe)

data Parse a = Failed String | Parsed a deriving (Show)

data NonEmpty a = NonEmpty a [a] deriving (Show)

pInt :: String -> Parse Int
pInt s = case readMaybe s of
  Just n -> Parsed n
  Nothing -> Failed ("Not an integer: " ++ s)

parseList :: [String] -> Parse [Int]
parseList [] = Parsed []
parseList (s : ss) =
  case pInt s of
    Failed err -> Failed err
    Parsed x ->
      case parseList ss of
        Failed err -> Failed err
        Parsed xs -> Parsed (x : xs)

-- >>> pSomeInts ["123", "345", "0"]
-- Parsed (NonEmpty 123 [345,0])
-- >>> pSomeInts ["123", "1a", "0"]
-- Failed "Not an integer: 1a"
-- >>> pSomeInts []
-- Failed "Input list is empty"
pSomeInts :: [String] -> Parse (NonEmpty Int)
pSomeInts [] = Failed "Input list is empty"
pSomeInts (x : xs) =
  case pInt x of
    Failed err -> Failed err
    Parsed headVal ->
      case parseList xs of
        Failed err -> Failed err
        Parsed tailVals -> Parsed (NonEmpty headVal tailVals)
