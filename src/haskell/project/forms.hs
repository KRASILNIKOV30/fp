import Control.Monad (ap)
import Data.List (intercalate)
import Data.Time (Day)
import Text.Read (readMaybe)

-- Вид ошибок при разборе ввода пользователя.
data InputError
  = EmptyInput -- Пустой ввод.
  | CannotParse String -- Ввод нельзя разобрать в значение.
  | InvalidInput String -- Ввод можно разобрать, но полученное значение не является корректным.
  deriving (Show)

-- Результат ввода пользователя.
data InputResult a
  = Success a
  | Failure InputError
  deriving (Show)

-- >>> textInput ""
-- Failure EmptyInput
-- >>> textInput "hello"
-- Success "hello"
textInput :: String -> InputResult String
textInput "" = Failure EmptyInput
textInput str = Success str

-- >>> numericInput "123"
-- Success 123
-- >>> numericInput ""
-- Failure EmptyInput
-- >>> numericInput "1.5"
-- Failure (CannotParse "1.5")
-- >>> numericInput "False"
-- Failure (CannotParse "False")
numericInput :: String -> InputResult Int
numericInput = readInput

-- >>> inputAsIs ""
-- Success ""
-- >>> inputAsIs "hello"
-- Success "hello"
inputAsIs :: a -> InputResult a
inputAsIs = Success

-- >>> readInput "(123,False)" :: InputResult (Int, Bool)
-- Success (123,False)
-- >>> readInput "" :: InputResult (Int, Bool)
-- Failure EmptyInput
-- >>> readInput "(15," :: InputResult (Int, Bool)
-- Failure (CannotParse "(15,")
-- >>> readInput "False" :: InputResult Bool
-- Success False
-- >>> readInput "123" :: InputResult Int
-- Success 123
readInput :: (Read a) => String -> InputResult a
readInput "" = Failure EmptyInput
readInput str = case readMaybe str of
  Just a -> Success a
  Nothing -> Failure (CannotParse str)

newtype Positive = Positive Int deriving (Show)

-- >>> positive 123
-- Just (Positive 123)
-- >>> positive (-123)
-- Nothing
-- >>> positive 0
-- Nothing
positive :: Int -> Maybe Positive
positive a
  | a > 0 = Just (Positive a)
  | otherwise = Nothing

-- >>> validateWith "Number must be positive" (> 0) (numericInput "-123")
-- Failure (InvalidInput "Number must be positive")
-- >>> validateWith "Number must be positive" (> 0) (numericInput "123")
-- Success 123
-- >>> validateWith "Number must be positive" (> 0) (numericInput "1.23")
-- Failure (CannotParse "1.23")
-- >>> validateWith "Number must be positive" (> 0) (numericInput "")
-- Failure EmptyInput
validateWith :: String -> (a -> Bool) -> InputResult a -> InputResult a
validateWith str pred = validateInput str cond
  where
    cond a = if pred a then Just a else Nothing

-- >>> validateInput "Number must be positive" positive (readInput "-123")
-- Failure (InvalidInput "Number must be positive")
-- >>> validateInput "Number must be positive" positive (readInput "123")
-- Success (Positive 123)
-- >>> validateInput "Number must be positive" positive (readInput "1.23")
-- Failure (CannotParse "1.23")
-- >>> validateInput "Number must be positive" positive (readInput "")
-- Failure EmptyInput
validateInput :: String -> (a -> Maybe b) -> InputResult a -> InputResult b
validateInput str cond (Success a) =
  case cond a of
    Just b -> Success b
    Nothing -> Failure (InvalidInput str)
validateInput _ _ (Failure err) = Failure err

-- >>> mapInputResult length (textInput "hello")
-- Success 5
-- >>> mapInputResult length (textInput "")
-- Failure EmptyInput
mapInputResult :: (a -> b) -> InputResult a -> InputResult b -- fmap
mapInputResult f (Success a) = Success (f a)
mapInputResult _ (Failure err) = Failure err

-- >>> combineInputResult (mapInputResult (+) (numericInput "2")) (numericInput "3")
-- Success 5
-- >>> combineInputResult (mapInputResult (+) (numericInput "2.1")) (numericInput "3")
-- Failure (CannotParse "2.1")
-- >>> combineInputResult (mapInputResult (+) (numericInput "2")) (numericInput "3.5")
-- Failure (CannotParse "3.5")
combineInputResult :: InputResult (a -> b) -> InputResult a -> InputResult b
combineInputResult (Success f) b = mapInputResult f b
combineInputResult (Failure err) _ = Failure err

-- >>> let f = \(xs :: [Int]) -> numericInput (concatMap show xs)
-- >>> bindInputResult (readInput "[1, 2, 3]") f
-- Success 123
bindInputResult :: InputResult a -> (a -> InputResult b) -> InputResult b
bindInputResult (Success a) f = f a
bindInputResult (Failure err) _ = Failure err

newtype Form a = Form {runForm :: [String] -> IO (InputResult a)}

prompt :: String -> IO String
prompt text = do
  putStr text
  getLine

breadcrumbs :: [String] -> String
breadcrumbs fields = "[" ++ intercalate " > " fields ++ "]: "

inputForm :: (String -> InputResult a) -> Form a
inputForm parser =
  Form
    ( \fields -> do
        let msg = breadcrumbs fields
        inputStr <- prompt msg
        return (parser inputStr)
    )

textForm :: Form String
textForm = inputForm textInput

numericForm :: Form Int
numericForm = inputForm numericInput

readForm :: (Read a) => Form a
readForm = inputForm readInput

printError :: InputError -> IO ()
printError err = case err of
  EmptyInput -> putStrLn "[Ошибка] Ввод не должен быть пустым (обязательное поле)."
  CannotParse str -> putStrLn ("[Ошибка] Непонятный ввод: " ++ str)
  InvalidInput str -> putStrLn ("[Ошибка] " ++ str)

retryForm :: [String] -> Form a -> IO a
retryForm fields form = do
  result <- runForm form fields
  case result of
    Success a -> return a
    Failure err -> do
      printError err
      retryForm fields form

retry :: Form a -> Form a
retry form =
  Form
    ( \fields -> do
        val <- retryForm fields form
        return (Success val)
    )

optionalForm :: Form a -> Form (Maybe a)
optionalForm form =
  Form
    ( \fields -> do
        result <- runForm form fields
        return
          ( case result of
              Failure EmptyInput -> Success Nothing
              Success a -> Success (Just a)
              Failure err -> Failure err
          )
    )

emptyForm :: a -> Form a
emptyForm val = Form (\_ -> return (Success val))

mapForm :: (a -> b) -> Form a -> Form b
mapForm f form =
  Form
    ( \fields -> do
        result <- runForm form fields
        return (mapInputResult f result)
    )

combineForm :: Form (a -> b) -> Form a -> Form b
combineForm formF formA =
  Form
    ( \fields -> do
        resF <- runForm formF fields
        resA <- runForm formA fields
        return (combineInputResult resF resA)
    )

listForm :: Int -> Form a -> Form [a]
listForm n form
  | n <= 0 = emptyForm []
  | otherwise = combineForm (mapForm (:) form) (listForm (n - 1) form)

bindForm :: Form a -> (a -> Form b) -> Form b
bindForm form f =
  Form
    ( \fields -> do
        result <- runForm form fields
        case result of
          Success a -> runForm (f a) fields
          Failure err -> return (Failure err)
    )

subform :: String -> Form a -> Form a
subform name form = Form (\fields -> runForm form (fields ++ [name]))

describe :: String -> Form a -> Form a
describe desc form =
  Form
    ( \fields -> do
        putStrLn desc
        runForm form fields
    )

validateForm :: String -> (a -> Maybe b) -> Form a -> Form b
validateForm msg predicate form =
  Form
    ( \fields -> do
        result <- runForm form fields
        return (validateInput msg predicate result)
    )

newtype Phone = Phone Integer deriving (Show)

validatePhone :: Integer -> Maybe Phone
validatePhone n
  | n >= 10000000000 && n <= 99999999999 = Just (Phone n)
  | otherwise = Nothing

phoneForm :: Form Phone
phoneForm =
  validateForm
    "Некорректный ввод: Ожидается номер в международном формате (11 цифр)."
    validatePhone
    readForm

newtype Email = Email String deriving (Show)

validateEmail :: String -> Maybe Email
validateEmail str
  | '@' `elem` str = Just (Email str)
  | otherwise = Nothing

emailForm :: Form Email
emailForm =
  validateForm
    "Некорректный ввод: Должен быть корректный адрес эл. почты."
    validateEmail
    textForm

field :: String -> Form a -> Form a
field = subform

textField :: String -> Form String
textField name = field name textForm

numericField :: String -> Form Int
numericField name = field name numericForm

readField :: (Read a) => String -> Form a
readField name = field name readForm

validateBool :: String -> String -> String -> Maybe Bool
validateBool trueStr falseStr input
  | input == trueStr = Just True
  | input == falseStr = Just False
  | otherwise = Nothing

boolForm :: String -> String -> Form Bool
boolForm trueStr falseStr = validateForm msg predicate textForm
  where
    msg = "Ответ должен быть [" ++ trueStr ++ "] или [" ++ falseStr ++ "] (без скобок)."
    predicate = validateBool trueStr falseStr

instance Functor Form where
  fmap = mapForm

instance Applicative Form where
  pure = emptyForm
  (<*>) = combineForm

instance Monad Form where
  (>>=) = bindForm

newtype FullName = FullName String deriving (Show)

data Role = Regular | Admin deriving (Show, Read)

data User = User
  { userName :: FullName,
    userPhone :: Phone,
    userBirthday :: Maybe Day,
    userEmail :: Maybe Email,
    userRole :: Role
  }
  deriving (Show)

shortName :: FullName -> String
shortName (FullName name) =
  case words name of
    [] -> ""
    (surname : rest) ->
      let initials = concatMap (\w -> take 1 w ++ ".") rest
       in surname ++ " " ++ initials

user :: Form User
user = do
  nameStr <- textField "ФИО"
  let fullName = FullName nameStr
  let short = shortName fullName
  subform
    short
    ( do
        phone <- field "Телефон" phoneForm
        birthday <- field "Дата рождения" (optionalForm readForm)
        email <- field "Email" (optionalForm emailForm)
        role <- field "Роль" readForm

        return (User fullName phone birthday email role)
    )

type Grade = Int

newtype CorrectAnswer a = CorrectIs a deriving (Show)

newtype Quiz a = Quiz {runQuiz :: Form (Grade, a)}

execQuiz :: Quiz a -> IO (Grade, a)
execQuiz q = retryForm [] (runQuiz q)

gradeWith :: (a -> Grade) -> Form a -> Quiz a
gradeWith grader form =
  Quiz
    ( do
        val <- form
        return (grader val, val)
    )

gradeCorrectAnswer :: (Eq a) => CorrectAnswer a -> Form a -> Quiz a
gradeCorrectAnswer (CorrectIs correct) =
  gradeWith (\val -> if val == correct then 1 else 0)

trueOrFalse :: String -> CorrectAnswer Bool -> Quiz Bool
trueOrFalse question correct = gradeCorrectAnswer correct (field "Ответ" form)
  where
    form = describe question (boolForm "да" "нет")

numerical :: String -> CorrectAnswer Int -> Quiz Int
numerical question correct = gradeCorrectAnswer correct form
  where
    form = describe question (numericField "Ответ")

singleChoice :: String -> [String] -> CorrectAnswer String -> Quiz String
singleChoice question options correct =
  gradeCorrectAnswer correct (describe fullDesc selectionForm)
  where
    formattedOptions = unlines (zipWith (\i opt -> show i ++ ") " ++ opt) [1 ..] options)
    count = length options
    fullDesc = question ++ "\n" ++ formattedOptions
    prompt = "Выберите верный вариант (от 1 до " ++ show count ++ ")"
    checkIndex i = if i >= 1 && i <= count then Just i else Nothing
    selectionForm =
      mapForm
        (\i -> options !! (i - 1))
        ( validateForm
            ("Ответ должен быть от 1 до " ++ show count ++ ".")
            checkIndex
            (numericField prompt)
        )

emptyQuiz :: a -> Quiz a
emptyQuiz val = Quiz (return (0, val))

mapQuiz :: (a -> b) -> Quiz a -> Quiz b
mapQuiz f (Quiz form) =
  Quiz
    ( do
        (grade, val) <- form
        return (grade, f val)
    )

bindQuiz :: Quiz a -> (a -> Quiz b) -> Quiz b
bindQuiz (Quiz formA) f =
  Quiz
    ( do
        (gradeA, valA) <- formA
        let (Quiz formB) = f valA
        (gradeB, valB) <- formB
        return (gradeA + gradeB, valB)
    )

instance Functor Quiz where
  fmap = mapQuiz

instance Applicative Quiz where
  pure = emptyQuiz
  (<*>) = ap

instance Monad Quiz where
  (>>=) = bindQuiz

-- Тест о Haskell
miniQuiz :: Quiz ()
miniQuiz = do
  _ <- trueOrFalse "В Haskell переменные изменяемые?" (CorrectIs False)

  _ <-
    singleChoice
      "Как добавить элемент 'x' в начало списка 'xs'?"
      ["x ++ xs", "x : xs", "xs + x", "add x xs"]
      (CorrectIs "x : xs")

  _ <-
    singleChoice
      "Что вернет head []?"
      ["Nothing", "Пустой список", "Ошибка", "0"]
      (CorrectIs "Ошибка")

  _ <- numerical "Сколько аргументов принимает функция map?" (CorrectIs 2)

  _ <- trueOrFalse "Haskell — лучший язык?" (CorrectIs True)

  return ()