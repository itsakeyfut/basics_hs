{-# LANGUAGE OverloadedStrings #-}
module Todo
    ( Todo(..)
    , TodoId
    , TodoList
    , createTodo
    , completeTodo
    , removeTodo
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(..))

type TodoId = Int
type TodoList = Map.Map TodoId Todo

data Todo = Todo
    { todoId        :: TodoId
    , todoTitle     :: Text
    , todoCompleted :: Bool
    , todoCreatedAt :: UTCTime
    } deriving (Show, Eq)

instance ToJSON Todo where
    toJSON todo = object
        [ "id" .= todoId todo
        , "title" .= todoTitle todo
        , "completed" .= todoCompleted todo
        , "createdAt" .= todoCreatedAt todo
        ]

instance FromJSON Todo where
    parseJSON (Object v) = Todo
        <$> v .: "id"
        <*> v .: "title"
        <*> v .: "completed"
        <*> v .: "createdAt"
    parseJSON _ = fail "Expected an object for Todo"

createTodo :: Text -> UTCTime -> TodoId -> Todo
createTodo title currentTime nextId = Todo
    { todoId = nextId
    , todoTitle = title
    , todoCompleted = False
    , todoCreatedAt = currentTime
    }

completeTodo :: Todo -> Todo
completeTodo todo = todo { todoCompleted = True }

removeTodo :: TodoId -> TodoList -> TodoList
removeTodo = Map.delete