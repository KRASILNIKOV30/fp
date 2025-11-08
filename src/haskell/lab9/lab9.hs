import Data.Char (toUpper, isLetter)
echo :: IO ()
echo = do
    line <- getLine
    print line
    echo

shoutEcho :: IO ()
shoutEcho = do
    line <- map toUpper <$> getLine
    print line
    shoutEcho

echoByWord :: IO ()
echoByWord = do
    line <- getLine
    putStrLn ((unwords . map (capitalize . (++ ".")) . words . map (\c -> if isLetter c then c else ' ')) line)
    echoByWord   
      where
        capitalize :: String -> String
        capitalize [] = []
        capitalize (x:xs) = toUpper x : xs