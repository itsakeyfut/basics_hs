numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

-- list concatenation
combined = [1, 2] ++ [3, 4]

-- cons (push front)
moreNumbers = 0 : numbers

-- pop
first = head numbers -- 1
rest  = tail numbers -- [2, 3, 4, 5]
lastN = last numbers -- 5

-- numbers powered by 2 list in range of 1 - 10
squares = [x * x | x <- [1..10]]

-- pop even numbers
evens = [x | x <- [1..20], even x]

-- filter chars
noVowels = [c | c <- "Hello World", not (c `elem` "aeiouAEIOU")]

-- nested list behaving like nested loop
pairs = [(x, y) | x <- [1, 2], y <- ['a', 'b']]
-- => [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]