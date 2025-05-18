{-# LANGUAGE OverloadedStrings #-}
module Main where

import Todo
import Storage
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)
import Data.List (sortOn)

main :: IO ()
main = do
  putStrLn "Welcome to Haskell Todo App!"
  todos <- loadTodos
  let nextId = if Map.null todos 
               then 1 
               else maximum (Map.keys todos) + 1
  appLoop todos nextId

-- メインアプリケーションループ
appLoop :: TodoList -> TodoId -> IO ()
appLoop todos nextId = do
  displayTodos todos
  putStrLn "\nCommands: add, complete [id], remove [id], quit"
  putStr "> "
  hFlush stdout
  cmd <- getLine
  case words cmd of
    ["add"] -> do
      putStr "Title: "
      hFlush stdout
      title <- T.pack <$> getLine
      currentTime <- getCurrentTime
      let newTodo = createTodo title currentTime nextId
          newTodos = Map.insert nextId newTodo todos
      saveTodos newTodos
      appLoop newTodos (nextId + 1)
      
    ["complete", idStr] -> do
      let todoId = read idStr :: TodoId
      case Map.lookup todoId todos of
        Just todo -> do
          let updatedTodo = completeTodo todo
              newTodos = Map.insert todoId updatedTodo todos
          saveTodos newTodos
          appLoop newTodos nextId
        Nothing -> do
          putStrLn "Todo not found!"
          appLoop todos nextId
          
    ["remove", idStr] -> do
      let todoId = read idStr :: TodoId
          newTodos = removeTodo todoId todos
      saveTodos newTodos
      appLoop newTodos nextId
      
    ["quit"] -> putStrLn "Goodbye!"
    
    _ -> do
      putStrLn "Unknown command!"
      appLoop todos nextId

displayTodos :: TodoList -> IO ()
displayTodos todos = do
  putStrLn "\nYour Todos:"
  if Map.null todos
    then putStrLn "  (no todos)"
    else mapM_ displayTodo (sortOn todoId $ Map.elems todos)
  where
    displayTodo todo = do
      let status = if todoCompleted todo then "[X]" else "[ ]"
      putStrLn $ "  " ++ status ++ " " ++ show (todoId todo) ++ ": " ++ T.unpack (todoTitle todo)