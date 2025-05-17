-- Type Annotation + Function Definition
square :: Int -> Int
square x = x * x

-- Two Arguments
add :: Int -> Int -> Int
add x y = x + y

-- Fibonacci (Recursive) 
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Tuple Pattern Matching
describePoint :: (Int, Int) -> String
describePoint (0, 0) = "Origin"
describePoint (x, 0) = "On the X-axis"
describePoint (0, y) = "On the Y-axis"
describePoint (x, y) = "Somewhere else"

-- List Pattern Matching
head' :: [a] -> a
head' [] error "Empty list!"
head' (x:_) = x