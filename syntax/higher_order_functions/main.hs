module Main where

main :: IO ()
main = do
    putStrLn "== map =="
    print $ map (*2) [1, 2, 3, 4]
    -- => [2, 4, 6, 8]

    putStrLn "== filter =="
    print $ filter even [1..10]
    -- => [2, 4, 6, 8, 10]

    putStrLn "== foldr =="
    print $ foldr (+) 0 [1, 2, 3, 4]
    -- => 10

    putStrLn "== foldl =="
    print $ foldl (*) 1 [1, 2, 3, 4]
    -- => 24

    putStrLn "== Complex =="
    print $ sum (map (^2) (filter even [1..5]))
    -- => 20 (2^2 + 4^2)