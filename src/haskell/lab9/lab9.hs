{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

import Control.Monad (forM_, when)
import Data.Char (isLetter, toUpper)
import Text.Read (readMaybe)

echo :: IO ()
echo = do
  line <- getLine
  putStrLn line
  echo

shoutEcho :: IO ()
shoutEcho = do
  line <- map toUpper <$> getLine
  putStrLn line
  shoutEcho

echoByWord :: IO ()
echoByWord = do
  line <- getLine
  putStrLn ((unwords . map (capitalize . (++ ".")) . words . map (\c -> if isLetter c then c else ' ')) line)
  echoByWord
  where
    capitalize :: String -> String
    capitalize [] = []
    capitalize (x : xs) = toUpper x : xs

confirmIO :: String -> (Bool -> String) -> IO Bool
confirmIO prompt answerFunc = do
  putStr (prompt ++ " ")
  userInput <- getLine

  let yesString = answerFunc True
  let noString = answerFunc False

  if userInput == yesString
    then
      return True
    else
      if userInput == noString
        then
          return False
        else do
          putStrLn ("Неверный ввод. Пожалуйста, используйте '" ++ yesString ++ "' или '" ++ noString ++ "'.")
          confirmIO prompt answerFunc

continueIO :: IO a -> IO ()
continueIO program = do
  program
  answer <- confirmIO "Продолжить?" (\b -> if b then "да" else "нет")
  when answer (continueIO program)

data Mode
  = Debug
  | Verbose
  | Normal
  | Silent
  deriving (Show, Eq, Ord)

verboseIO :: Mode -> IO () -> IO ()
verboseIO mode = when (mode <= Verbose)

maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO (Just action) = Just <$> action
maybeIO Nothing = return Nothing

data Result a
  = Success a
  | Failure String
  deriving (Show, Eq)

sequenceResultIO :: [IO (Result a)] -> IO [a]
sequenceResultIO [] = return []
sequenceResultIO (action : actions) = do
  result <- action
  case result of
    Success v -> (v :) <$> sequenceResultIO actions
    Failure err -> do
      putStrLn ("[Failure]: " ++ err)
      sequenceResultIO actions

data Step a = Stop | Next a

unfoldIO :: (a -> IO (Step a)) -> a -> IO [a]
unfoldIO f init = (init :) <$> do
  res <- f init
  case res of
    Next a -> unfoldIO f a
    Stop -> return []

{-# HLINT ignore "Use foldM" #-}
forStateIO :: s -> [a] -> (s -> a -> IO s) -> IO s
forStateIO s [] f = return s
forStateIO s (x : xs) f = do
  newS <- f s x
  forStateIO newS xs f

printConsLength :: Int -> String -> IO Int
printConsLength totalLength s = do
  let n = length s
      newTotalLength = totalLength + n
  putStrLn
    ( "добавляем "
        ++ show s
        ++ " (длина = "
        ++ show newTotalLength
        ++ ")"
    )
  return newTotalLength

-- >>> forStateIO 0 ["привет", "мир", "!"] printConsLength
-- 10

iforIO :: [a] -> (Int -> a -> IO b) -> IO [b]
iforIO xs f = helper 0 xs
  where
    helper _ [] = return []
    helper i (x : xs) = do
      res <- f i x
      (res :) <$> helper (i + 1) xs

example :: IO [[[(Int, Char)]]]
example = do
  iforIO
    [1, 2]
    ( \i n ->
        iforIO
          "ab"
          ( \j c -> do
              print ((i, j), replicate n c)
              return [(n, c)]
          )
    )