module Exercises where
import Data.Char (isDigit, digitToInt, isSpace)

-- ================================================================= --
-- Упражнение 8.1 (Инстанциирование переменных типов)
-- ================================================================= --


twice :: (a -> a) -> a -> a
twice f x = f (f x)

dup :: (b -> b -> c) -> b -> c
dup f x = f x x

-- >>> twice (+1) 0
-- 2

-- >>> twice (++ "!") "Hello"
-- "Hello!!"

-- >>> dup (+) 123
-- 246
   -- `(+)` имеет тип `Num n => n -> n -> n`.
   -- `b` и `c` в типе `dup` инстанциируются с `Int`.


-- >>> dup (dup (++)) "Hello"
   --Внутреннее выражение `dup (++)` имеет тип `[a] -> [a]`.
   -- Внешний `dup` ожидает функцию двух аргументов (типа `b -> b -> c`),
   -- а получает функцию одного аргумента.


-- >>> twice (dup (.) (+1)) 0
-- 4
   -- `dup (.) (+1)` создает функцию `(+1) . (+1)`, то есть `\x -> x + 2`.
   -- `twice` применяет эту функцию дважды: `(\x -> x + 2) ((\x -> x + 2) 0)` -> `(\x -> x + 2) 2` -> `4`.
   -- `a` инстанциируется с `Int`.


-- >>> twice dup
-- ОШИБКА ТИПА
   -- `twice` ожидает функцию типа `a -> a`. Тип `dup`
   -- `(b -> b -> c) -> b -> c` не может быть сведен к `a -> a`.


-- >>> twice twice (+1) 0
-- 4
   -- Выражение `twice twice` применяет функцию 4 раза


-- >>> twice twice twice (+1) 0
-- 16



-- ================================================================= --
-- Упражнение 8.2 (Подсчёт возможных реализаций)
-- ================================================================= --

-- 1.
-- (a) f1 :: Int -> Int
-- Много реализаций, так как `Int` - конкретный тип.
f1_example1, f1_example2 :: Int -> Int
f1_example1 x = x * 2
f1_example2 _ = 42

-- (b) f2 :: a -> a
-- Одна реализация. Мы ничего не знаем о типе `a`, поэтому можем только вернуть то, что получили.
f2 :: a -> a
f2 = id

-- (c) f3 :: a -> Int
-- Много реализаций. Мы не можем использовать значение типа `a` для получения `Int`,
-- поэтому можем только возвращать константу.
f3_example1, f3_example2 :: a -> Int
f3_example1 _ = 0
f3_example2 _ = 123

-- (d) f4 :: Int -> a
-- f4 :: Int -> a
-- f4 = undefined

-- 2.
-- (a) g1 :: (a, b) -> a
g1 :: (a, b) -> a
g1 = fst

-- (b) g2 :: (a, b) -> b
g2 :: (a, b) -> b
g2 = snd

-- (c) g3 :: (a, a) -> a
g3_first, g3_second :: (a, a) -> a
g3_first = fst
g3_second = snd

-- 3.
-- (a) h1 :: Bool -> Bool
-- Четыре реализации (2^2).
h1_id, h1_not, h1_true, h1_false :: Bool -> Bool
h1_id x = x
h1_not = not
h1_true _ = True
h1_false _ = False

-- (b) h2 :: Maybe a -> Bool
h2_isJust, h2_isNothing, h2_true, h2_false :: Maybe a -> Bool
h2_isJust (Just _) = True
h2_isJust Nothing = False
h2_isNothing = not . h2_isJust
h2_true _ = True
h2_false _ = False

-- (c) h3 :: Maybe a -> Maybe a
-- Две реализации. Нельзя создать `Just a` из ничего.
h3_id, h3_constNothing :: Maybe a -> Maybe a
h3_id mx = mx
h3_constNothing _ = Nothing

-- 4.
-- (a) k1 :: (a -> b) -> a -> b
-- Одна реализация. Единственный способ получить `b` - применить функцию к значению.
k1 :: (a -> b) -> a -> b
k1 = ($) -- Это оператор `($)`

-- (b) k2 :: (a -> a) -> a -> a
-- Бесконечно много реализаций. Можно применять функцию `f` любое количество раз.
k2_0, k2_1, k2_2 :: (a -> a) -> a -> a
k2_0 f x = x
k2_1 f = f
k2_2 f x = f (f x)

-- (c) k3 :: (a -> a) -> a -> [a]
-- Бесконечно много реализаций. Можно сгенерировать список любой длины.
k3_empty, k3_singleton, k3_iterate :: (a -> a) -> a -> [a]
k3_empty     f x = []
k3_singleton f x = [x, x]
k3_iterate   f x = x : k3_iterate f (f x)

-- 5.
-- (a) t1 :: (a -> b) -> [a] -> [b]
t1 :: (a -> b) -> [a] -> [b]
t1 = map

-- (b) t2 :: (a -> Bool) -> [a] -> [a]
t2 :: (a -> Bool) -> [a] -> [a]
t2 = filter


-- ================================================================= --
-- Упражнение 8.3 (Работа на результат)
-- ================================================================= --

data Result a
  = Success a
  | Failure String
  deriving (Show, Eq)

-- 1. Перевести строку в число (не используя read!):

-- >>> parseInt "123"
-- Success 123
-- >>> parseInt "123asd"
-- Failure "not a number: 123asd"
-- >>> parseInt ""
-- Failure "not a number (empty string)"
parseInt :: String -> Result Int
parseInt str
  | null str = Failure "not a number (empty string)"
  | not (all isDigit str) = Failure ("not a number: " ++ str)
  | otherwise = Success (foldl (\acc char -> acc * 10 + digitToInt char) 0 str)

-- 2. Перевести список строк в список чисел:

-- >>> parseInts ["123","345","0"]
-- Success [123,345,0]
-- >>> parseInts ["123","345x","0"]
-- Failure "not a number: 345x"
parseInts :: [String] -> Result [Int]
parseInts [] = Success []
parseInts (s:ss) =
  case parseInt s of
    Failure msg -> Failure msg
    Success n ->
      case parseInts ss of
        Failure msg -> Failure msg
        Success ns  -> Success (n : ns)

-- 3. Перевести строку в список чисел:

-- >>> parseListOfInt "[123, 345, 0]"
-- Success [123,345,0]
-- >>> parseListOfInt "[123, 345x, 0]"
-- Failure "not a number: 345x"
-- >>> parseListOfInt " [ 1, 2, 3 ] "
-- Success [1,2,3]
-- >>> parseListOfInt "не список"
-- Failure "incorrect list format"
parseListOfInt :: String -> Result [Int]
parseListOfInt str
  | not (startsWith "[" trimmed && endsWith "]" trimmed) = Failure "incorrect list format"
  | otherwise =
      let
        content = (drop 1 . take (length trimmed - 1)) trimmed
        numberStrings = map trim (splitOn ',' content)
      in
        parseInts numberStrings
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    trimmed = trim str
    startsWith p s = p == take (length p) s
    endsWith p s = p == take (length p) (reverse s)
    splitOn _ "" = []
    splitOn delim s =
      let (segment, rest) = break (== delim) s
      in segment : case rest of
                      [] -> []
                      (_:xs) -> splitOn delim xs

-- 4. Применить функцию к результату:

-- >>> mapResult (+1) (parseInt "123")
-- Success 124
-- >>> mapResult length (parseListOfInt "[123, 345, 0]")
-- Success 3
-- >>> mapResult (*2) (Failure "error")
-- Failure "error"
mapResult :: (a -> b) -> Result a -> Result b
mapResult f (Success x)   = Success (f x)
mapResult _ (Failure msg) = Failure msg

-- 5. Применить функцию-результат к аргументу-результату:

-- >>> applyResult (mapResult (+) (parseInt "123")) (parseInt "345")
-- Success 468
-- >>> applyResult (Success (+10)) (Failure "error")
-- Failure "error"
-- >>> applyResult (Failure "error") (Success 10)
-- Failure "error"
applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Success f) (Success x)   = Success (f x)
applyResult (Failure msg) _ = Failure msg
applyResult _ (Failure msg) = Failure msg
