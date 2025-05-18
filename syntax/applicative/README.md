# Applicatite

関数が「構造」に含まれている場合の関数適用

## Applicative 型クラス定義

```hs
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

## 用語の意味

- `pure`：値を構造（`f`）に持ち上げる。Rust で言えば `Some(x)` や `Ok(x)` に包むようなもの。
- `<*>`：構造の中にある関数 `f (a -> b)` を、同じ構造にある値 `f a` に適用する。

## 直感的な例：Maybe

```hs
pure (+3) <*> Just 4  -- Just 7
Just (*2) <*> Just 10 -- Just 20
Nothing <*> Just 10   -- Nothing
```

```hs
main :: IO ()
main = do
    print $ pure (+1) <*> Just 5
    print $ Just (*2) <*> Just 4
    print $ Nothing <*> Just 4
```

## 裏側でどう動いている？

```hs
instance Applicative Maybe where
    pure = Just
    Just f <*> Just x = Just (f x)
    _ <*> _ = Nothing
```

- `Just f <*> Just x` → `Just (f x)`
- どちらかが `Nothing` の場合は結果も `Nothing` （失敗伝播）

## Functor との違い

| 操作   | 構造に値を適用 | 構造に関数を適用 |
| ------ | -------------- | ---------------- |
| `fmap` | ✅             | ❌               |
| `<*>`  | ✅             | ✅               |

```hs
-- fmap: 関数を普通の値に適用
fmap (+1) (Just 3) -- Just 4

-- <*>: 関数が Just に包まれている
Just (+1) <*> Just 3 -- Just 4
```

## リスト（[]）の場合

```hs
main :: IO ()
main = do
    print $ pure (*2) <*> [1, 2, 3] -- [2, 4, 6]
    print $ [(+1), {*2}] <*> [10, 20] -- [11, 21, 20, 40]
```

- `<*>`は「直積（全ての関数 x 全ての値）」になる

```hs
-- [(+1), (*2)] <*> [10, 20]
-- => [ (+1) 10, (+1) 20, (*2) 10, (*2) 20 ]
-- => [11, 21, 20, 40]
```

## 自作 Box 型での Applicative

```hs
module Main where

data Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure x = Box x
    Box f <*> Box x = Box (f x)

main :: IO ()
main = print $ Box (+2) <*> Box 3 -- Box 5
```

## Rust との比較

```rs
Some(|x| x + 1).and_then(|f| Some(f(3))); // Some(4)
```

- Rust には `Applicative` に完全一致する機能はないが、`Option` や `Result` を使えば類似の動きはできる
- `.zip_with()` で複数の値を同時に扱うことに近い

## Applicative のまとめ

| 概念           | 役割                                         |
| -------------- | -------------------------------------------- |
| `pure`         | 値を構造に持ち上げる                         |
| `<*>`          | 構造に包まれた関数を、構造に包まれた値に適用 |
| Functor との差 | 関数自体も構造の中にある場合に処理できる     |
