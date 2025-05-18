module Main where

-- Recursive Tree Data Structure
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

-- Convert elements into List in-order
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

-- Compute Tree Height
treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- Test tree
testTree :: Tree Int
testTree = Node 10
            (Node 5 Empty (Node 7 Empty Empty))
            (Node 15 Empty Empty)

main :: IO ()
main = do
    putStrLn "== Binary Tree =="
    print testTree

    putStrLn "\n== In-order Traversal =="
    print $ inOrder testTree

    putStrLn "\n== Tree Height =="
    print $ treeHeight testTree