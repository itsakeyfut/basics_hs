module Main where

data Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure x = Box x
    Box f <*> Box x = Box (f x)

main :: IO ()
main = print $ Box (+2) <*> Box 3 -- Box 5