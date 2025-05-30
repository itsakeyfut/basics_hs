# 高階関数（Higher-Order Functions）

高階関数とは、関数を引数に取ったり、関数を返したりする関数のことです。
Haskell では高階関数が非常に強力で、関数型プログラミングの中心的な要素です。

```hs
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
```

## Description

- `map`
  - `map f xs` は、リスト `xs` の各要素に関数 `f` を適用した新しいリストを返します。
  - 例：`map (*2) [1, 2, 3]` → `[2, 4, 6]`
- `filter`
  - `filter p xs` は、述語関数 `p` に合格した要素だけを抽出します。
  - 例：`filter even [1..5]` → `[2, 4]`
- `foldr` / `foldl`
  - `foldr`（右畳み込み）：`foldr f acc [x1, x2, ..., xn]` = `x1 f (x2 f (... (xn f acc)))`
  - `foldl`（左畳み込み）：`foldl f acc [x1, x2, ..., xn]` = `(...((acc f x1) f x2)...) f xn`
  - 例：
  ```hs
  foldr (+) 0 [1, 2, 3] -- => 6
  foldl (*) 1 [1, 2, 3] -- => 6
  ```

## Point

- Haskell のリスト処理は `map` → `filter` → `fold` の流れで行うと読みやすくなります。

## Question

### `foldr (+) 0 [...]` の `0` や `foldl (*) 1 [...]` の `1` は何を指すのか？

これは「初期値（accumulator の初期値）」です。

例：

```hs
foldr (+) 0 [1, 2, 3, 4]
```

これは次のように評価されます。

```hs
1 + (2 + (3 + (4 + 0))) = 10
```

つまり、右側から畳みこんでいく（`foldr`）ので、最後に `0` を足すことで最終的な加算が完成します。

```hs
foldl (*) 1 [1, 2, 3, 4]
```

これは次のように評価されます：

```hs
(((1 * 1) * 2) * 3) * 4 = 24
```

ここでは左側から畳みこんでいく（`foldl`）ので、最初の `1` は積の初期値（単位元）として使われています。

### まとめ

| 関数    | 目的           | 初期値の意味         |
| ------- | -------------- | -------------------- |
| `foldr` | 右から畳み込む | 最後に使う値         |
| `foldl` | 左から畳み込む | 最初の計算スタート値 |

- `+` なら初期値は `0`（加算の単位元）
- `*` なら初期値は `1`（乗算の単位元）
- `++`（文字列結合）なら初期値は `""`（空文字）

### `foldr` と `foldl` の違いがわかりにくい

`foldr (+) 0 [1, 2, 3, 4]`

```hs
1 + (2 + (3 + (4 + 0)))
-- 結果: 10
```

`foldl (+) 0 [1, 2, 3, 4]`

```hs
(((0 + 1) + 2) + 3) + 4
-- 結果: 10
```

両者は加算のような結合側を持つ関数なら結果は同じですが、非結合な処理（例：文字列構築）では結果が異なることがあります。

### 特徴

| 関数     | 処理                            | 例                           |
| -------- | ------------------------------- | ---------------------------- |
| `map`    | 各要素に関数を適用する          | `map (+1) [1,2,3] → [2,3,4]` |
| `filter` | 条件に合う要素を抽出する        | `filter even [1..5] → [2,4]` |
| `fold`   | リスト全体を 1 つの値にまとめる | `foldl (+) 0 [1,2,3] → 6`    |

`fold` は、リストの「集約」処理に使うのが基本です。

例：

```hs
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []
```
