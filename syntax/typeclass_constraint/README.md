# 型クラス制約と標準型クラスの概要

Haskell の型クラスは、Rust のジェネリクス + トレイト境界に相当するもので、関数の型シグネチャに「この型はこのクラスのインスタンスであるべき」と制限を加えることができます。

## 型クラス制約のある関数

```hs
contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains x (y:ys) = x == y || contains x ys
```

- `Eq a =>` の部分が「型 `a` は `Eq` 型クラスのインスタンスである必要がある」ことを示しています。
- Rust で言えば：
  `fn contains<T: Eq>(x: T, list: &[T]) -> bool`

## よく使う標準型クラスの一覧

### `Eq`

- 等価性（==, /=）
- 必要条件：比較可能な型にする

### `Ord`

- 順序関係（<, <=, >, >=）
- 例：`sort :: Ord a => [a] -> [a]`

```hs
import Data.List (sort)

main :: IO ()
main = print $ sort [3, 1, 2] -- [1, 2, 3]
```

### `Num`

- 数値型：`+`, `-`, `*`, `abs`, `signum`, `fromInteger`
- 例：`double :: Num a => a -> a`

```hs
double :: Num a => a -> a
double x = x * 2

main :: IO ()
main = print $ double 3.5
```

## `Show` / `Read`

- `Show`：値 → 文字列（`show`）
- `Read`：文字列 → 値（`read`）

```hs
main :: IO ()
main = do
    print $ show 123          -- "123"
    print $ read "123" :: Int -- 123
```

## Rust との対応早見表

| Haskell 型クラス | Rust の対応概念                              |
| ---------------- | -------------------------------------------- |
| `Eq`             | `PartialEq`, `Eq`                            |
| `Ord`            | `PartialOrd`, `Ord`                          |
| `Show`           | `Display`, `Debug`                           |
| `Read`           | `FromStr`                                    |
| `Num`            | 数値型トレイト群（`Add`, `Sub`, `Mul` など） |
