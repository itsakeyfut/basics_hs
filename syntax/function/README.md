# 関数定義とパターンマッチ（Function Definitions and Pattern Matching）

Haskell では関数が非常に中心的な存在です。
関数定義はシンプルで、「パターンマッチ」によって引数の形に応じた処理が可能です。

```hs
-- Type Annotation + Function Definition
square :: Int -> Int
square x = x * x

-- Two Arguments
add :: Int -> Int -> Int
add x y = x + y
```

```hs
-- Type Annotation + Function Definition
square :: Int -> Int
square x = x * x

-- Two Arguments
add :: Int -> Int -> Int
add x y = x + y

-- Fibonacci (Recursive)
fib ::Int -> Int
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
```

## Description

- `fib` のように、同じ関数名で複数の定義が可能で、引数の形によってマッチしたものが使われます。
- タプル `(x, y)` やリスト `x:xs` のように、構造が直接分解できます。
- パターンマッチの順序は上から評価され、最初にマッチしたものが使われます。

## Point

- Haskell では関数も値（ファーストクラス）なので、変数に関数を束縛したり、高階関数の引数に渡すこともできます。
- リストやタプルの分解は非常に頻出します。特にリストの `x:xs` （先頭と残りのリスト）はよく使います。
