module Main where

data Color = Red | Green | Blue deriving (Show)

data Vec2 = Vec2 Float Float deriving (Show)

-- Pattern Matching - Enum
describeColor :: Color -> String
describeColor Red   = "This is red."
describeColor Green = "This is green."
describeColor Blue  = "This is blue."

-- Pattern Matching - Struct
lengthVec2 :: Vec2 -> Float
lengthVec2 (Vec2 x y) = sqrt (x * x + y * y)

main :: IO ()
main = do
    putStrLn "== Color Enum =="
    print Red
    putStrLn $ describeColor Blue

    putStrLn "\n== Vec2 Struct =="
    let v = Vec2 3 4
    print v
    putStrLn $ "Length: " ++ show (lengthVec2 v)