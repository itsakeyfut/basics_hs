{-# LANGUAGE OverloadedStrings #-}
module Storage
    ( saveTodos
    , loadTodos
    ) where

import Todo (TodoList)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

todoFile :: FilePath
todoFile = "todos.json"

saveTodos :: TodoList -> IO ()
saveTodos todos = BL.writeFile todoFile (encode todos)

loadTodos :: IO TodoList
loadTodos = do
    exists <- doesFileExist todoFile
    if exists
        then do
            content <- BL.readFile todoFile
            let maybeTodos = decode content
            return (fromMaybe Map.empty maybeTodos)
        else
            return Map.empty
