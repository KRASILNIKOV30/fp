import Control.Monad (forM_)
import Text.Read (readMaybe)

newtype Item = Item {getItem :: String}

newtype TodoState = TodoState {getTodoState :: [Item]}

data Request
  = Exit
  | ListItems
  | AddItem Item
  | RemoveItem Int

printTasks :: [Item] -> IO ()
printTasks [] = putStrLn "Нет дел (всё сделано)."
printTasks tasks = mapM_ (putStrLn . getItem) tasks

parseRequest :: String -> Maybe Request
parseRequest input = case words input of
  ["выход"] -> Just Exit
  ["дела"] -> Just ListItems
  ["сделано", strIndex] -> case readMaybe strIndex of
    Nothing -> Nothing
    Just n -> Just (RemoveItem n)
  _ -> Just (AddItem (Item input))

data Response a = Response String (Maybe a)

type Handler = TodoState -> Response TodoState

readonly :: (TodoState -> String) -> Handler
readonly f state = Response (f state) (Just state)

handleListItems :: Handler
handleListItems = readonly (prettyItems . getTodoState)
  where
    prettyItems [] = "Нет дел (всё сделано)."
    prettyItems tasks = unlines (map getItem tasks)

handleAddItem :: Item -> Handler
handleAddItem item state =
  Response response (Just newState)
  where
    response = "Добавлено дело: " ++ getItem item
    TodoState items = state
    newState = TodoState (item : items)

deleteAt :: Int -> [a] -> Maybe (a, [a])
deleteAt _ [] = Nothing
deleteAt 0 (x : xs) = Just (x, xs)
deleteAt n (x : xs) = do
  (removed, rest) <- deleteAt (n - 1) xs
  return (removed, x : rest)

handleRemoveItem :: Int -> Handler
handleRemoveItem i state =
  case deleteAt i items of
    Just (removedItem, items') ->
      Response (responseSuccess removedItem) (Just (TodoState items'))
    Nothing -> Response responseFailure (Just state)
  where
    TodoState items = state
    responseSuccess (Item name) = "Дело выполнено: " ++ name
    responseFailure = "Дела номер " ++ show i ++ " не существует"

handleExit :: Handler
handleExit _state = Response "Пока!" Nothing

handleRequest :: Request -> Handler
handleRequest request =
  case request of
    Exit -> handleExit
    ListItems -> handleListItems
    AddItem item -> handleAddItem item
    RemoveItem i -> handleRemoveItem i

todoApp :: TodoState -> IO ()
todoApp state = do
  putStrLn "Введите запрос:"
  input <- getLine
  case parseRequest input of
    Nothing -> do
      putStrLn "Не получилось понять ваш запрос :("
      todoApp state
    Just request ->
      case handleRequest request state of
        Response response nextState -> do
          putStrLn response
          forM_ nextState todoApp

main = todoApp (TodoState [])