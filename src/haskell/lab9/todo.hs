{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Exception (SomeException, try)
import Control.Monad (forM_, (>=>))
import System.IO (IOMode (ReadMode), hFileSize, hFlush, stdout, withFile)
import Text.Read (readMaybe)

newtype Item = Item {getItem :: String} deriving (Show, Read)

newtype TodoState = TodoState {getTodoState :: [Item]} deriving (Show, Read)

-- добавить currentState
data AppState = AppState {getHistory :: [TodoState]} deriving (Show, Read)

initialState :: AppState
initialState = AppState [TodoState []]

saveFile :: FilePath
saveFile = "todo-state.txt"

data Request
  = ListItems
  | AddItem Item
  | RemoveItem Int
  | Save
  | Load
  | Undo
  | Exit

parseRequest :: String -> Maybe Request
parseRequest input = case words input of
  ["выход"] -> Just Exit
  ["дела"] -> Just ListItems
  ["сохранить"] -> Just Save
  ["загрузить"] -> Just Load
  ["отмена"] -> Just Undo
  ["сделано", strIndex] -> RemoveItem <$> readMaybe strIndex
  _
    | not (null input) -> Just (AddItem (Item input))
    | otherwise -> Nothing

data Response a = Response String (Maybe a)

type Handler = AppState -> Response AppState

updateState :: String -> AppState -> Response AppState
updateState msg newState = Response msg (Just newState)

handleListItems :: Handler
handleListItems state@(AppState (current : _)) =
  let msg = case getTodoState current of
        [] -> "Нет дел (всё сделано)."
        tasks -> unlines (zipWith (\n (Item t) -> show n ++ ". " ++ t) [1 ..] tasks)
   in Response msg (Just state)
handleListItems state = Response "История пуста." (Just state)

modifyCurrentState :: (TodoState -> TodoState) -> AppState -> AppState
modifyCurrentState f (AppState (current : history)) = AppState (f current : current : history)
modifyCurrentState _ (AppState []) = initialState

handleAddItem :: Item -> Handler
handleAddItem item state =
  updateState response (modifyCurrentState updateFunc state)
  where
    response = "Добавлено дело: " ++ getItem item
    updateFunc (TodoState items) = TodoState (item : items)

deleteAt :: Int -> [a] -> Maybe (a, [a])
deleteAt _ [] = Nothing
deleteAt n xs | n < 0 = Nothing
deleteAt 0 (x : rest) = Just (x, rest)
deleteAt n (x : rest) = do
  (removed, remaining) <- deleteAt (n - 1) rest
  return (removed, x : remaining)

handleRemoveItem :: Int -> Handler
handleRemoveItem i state@(AppState (current@(TodoState items) : _)) =
  case deleteAt (i - 1) items of
    Just (removedItem, items') ->
      let response = "Дело выполнено: " ++ getItem removedItem
          newState = TodoState items'
       in updateState response (modifyCurrentState (const newState) state)
    Nothing ->
      let response = "Дела номер " ++ show i ++ " не существует"
       in Response response (Just state)
handleRemoveItem _ state = Response "Нет дел для удаления." (Just state)

handleUndo :: Handler
handleUndo state@(AppState history) =
  case history of
    (_ : previous : rest) -> updateState "Последнее действие отменено." (AppState (previous : rest))
    _ -> Response "Нечего отменять." (Just state)

handlePureRequest :: Request -> Handler
handlePureRequest request =
  case request of
    ListItems -> handleListItems
    AddItem item -> handleAddItem item
    RemoveItem i -> handleRemoveItem i
    Undo -> handleUndo
    _ -> Response "Ошибка" . Just

saveStateToFile :: AppState -> IO ()
saveStateToFile (AppState (current : _)) = writeFile saveFile (show current)
saveStateToFile _ = return ()

loadStateFromFile :: IO (Maybe AppState)
loadStateFromFile = do
  result <- try (readFile saveFile) :: IO (Either SomeException String)
  case result of
    Left _ -> return Nothing
    Right content -> return (AppState . (: []) <$> readMaybe content)

todoApp :: AppState -> IO ()
todoApp state = do
  putStr "\n> "
  input <- getLine
  case parseRequest input of
    Nothing -> do
      putStrLn "Неверная команда. Доступные: дела, <текст>, сделано <номер>, отмена, сохранить, загрузить, выход"
      todoApp state
    Just request ->
      case request of
        Save -> do
          saveStateToFile state
          putStrLn "Состояние сохранено."
          todoApp state
        Load -> do
          loaded <- loadStateFromFile
          case loaded of
            Just loadedState -> do
              putStrLn "Состояние загружено."
              todoApp loadedState
            Nothing -> do
              putStrLn "Не удалось загрузить состояние."
              todoApp state
        Exit -> do
          putStrLn "Сохранение перед выходом..."
          saveStateToFile state
          putStrLn "Пока!"
        _ -> do
          let Response msg nextState = handlePureRequest request state
          putStrLn msg
          forM_ nextState todoApp

loadInitialState :: IO AppState
loadInitialState = do
  fileExists <- try (withFile saveFile ReadMode (hFileSize >=> (\s -> return $ s > 0))) :: IO (Either SomeException Bool)
  case fileExists of
    Right True -> do
      putStrLn $ "Найден файл состояния (" ++ saveFile ++ "). Загрузка..."
      loaded <- loadStateFromFile
      case loaded of
        Just state -> return state
        Nothing -> do
          putStrLn "Файл состояния поврежден. Начинаем с чистого листа."
          return initialState
    _ -> do
      putStrLn "Файл состояния не найден. Начинаем с чистого листа."
      return initialState

main :: IO ()
main = do
  initial <- loadInitialState
  todoApp initial