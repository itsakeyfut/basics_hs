# ユーザー定義のデータ型（Algebraic Data Types, ADTs）とパターンマッチング

Haskell では、独自のデータ型を定義できることが非常に重要です。

## `data` によるデータ型定義

```hs
-- Color Data Type
data Color = Red | Green | Blue deriving (Show)

-- Vector (x, y) Struct
data Vec2 = Vec2 Float Float deriving (Show)
```

- `Color` は列挙型です。
- `Vec2` はレコード型に近く、2 つの値を持ちます。

```hs
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
```

## Description

`data` 構文

```hs
data 型名 = コンストラクタ1 | コンストラクタ2 | ...
```

- 値コンストラクタ（`Red`, `Vec2`）は、関数としても使える。
- `deriving (Show)` を付けると、`print` で出力できるようになります。

パターンマッチング

- `case` 文の代わりに関数定義で直接マッチングできます。
- `Vec2 x y` のように、コンストラクタの内部にある値を取り出して使えます。

## Point

- Haskell のすべての構文解析・抽象構文木・DSL は、この `data` とパターンマッチングを基礎に組み立てます。

## Question

### 文字列リテラルを出力する関数

```hs
putStrLn "Hello, world!"
```

- `putStrLn` は `String -> IO ()` 型の関数です。
- 与えられた文字列に自動で改行（`\n`）を付けて標準出力に表示します。
- `String` に限定されており、文字列しか受け取れません。

### 値を文字列に変換して出力

```hs
print Red
```

- `print` は任意の型（ただし Show インスタンスを持つ型）を表示できます。
- 型クラス `Show` に属するものであれば、自動的に `show` で文字列に変換して出力します。
- 実際には以下と同じ意味です：

```hs
putStrLn (show Red)
```

### 関数適用の簡略記法

```hs
putStrLn $ describeColor Blue
```

これは以下と同じ意味です：

```hs
putStrLn (describeColor Blue)
```

`($)` は関数適用演算子で、右側をカッコで囲う代わりに使える糖衣構文（シンタックスシュガー）です。

#### `$` の型

```hs
($) :: (a -> b) -> a -> b
```

つまり、関数 `f` と引数 `x` に対して：

```hs
f $ x == f x
```

```hs
putStrLn (show (sum [1, 2, 3]))
-- ↓ こう書ける
putStrLn $ show $ sum [1, 2, 3]
```

### まとめ

| 式                | 説明                                                    |
| ----------------- | ------------------------------------------------------- |
| `putStrLn "text"` | 文字列リテラルを出力（型：`String -> IO ()`）           |
| `print x`         | 値 `x` を `show` して出力（型：`Show a => a -> IO ()`） |
| `putStrLn $ f x`  | `f x` を先に評価し、その結果を出力                      |
| `putStrLn (f x)`  | `$` の代わりにカッコを使った書き方                      |
