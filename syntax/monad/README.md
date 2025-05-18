# Monad

連続する処理を構造を保ちながら繋げる抽象

## Monad 型クラス定義

```hs
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b -- bind
    return :: a -> m a                -- 値をモナドに持ち上げる（`pure` とほぼ同じ）
```

- `>>=`（バインド演算子）は「モナドから値を取り出して関数に渡し、その結果もモナドになる」ことを表す
- `return` は普通の値をモナドに包む

## 直感的な例：Maybe モナド

```hs
module Main where

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

main :: IO ()
main = do
    print $ Just 10 >>= \x -> safeDiv x 2 -- Just 5
    print $ Just 10 >>= \x -> safeDiv x 0 -- Nothing
    print $ Nothing >>= \x -> safeDiv x 2 -- Nothing
```

- `Just 10 >>= \x -> safeDiv x 2` は `safeDiv 10 2` と同じ結果の `Just 5`
- どこかで `Nothing` が出たら、それ以降の処理はスキップされて結果は `Nothing`

## Monad のポイント

- 連続した処理（例：失敗を伝播させる処理）を「綺麗に」書ける
- 副作用を扱うのも Monad で一般的（例：`IO` モナド）
- `>>=` は「モナドから値を取り出し、次のモナド生成関数に渡す」演算子

## Maybe の Monad インスタンス

```hs
instance Monad Maybe where
    Just x >>= f = f x
    Nothing >>= _ = Nothing
    return = Just
```

## do 記法（モナドの糖衣構文）

```hs
main :: IO ()
main = do
    let safeDiv :: Int -> Int -> Maybe Int
        safeDiv _ 0 = Nothing
        safeDiv x y = Just (x `div` y)

    result <- Just 10 >>= \x -> safeDiv x 2
    print result

    -- do 記法で書くと
    res <- do
        x <- Just 10
        safeDiv x 2
    print res
```

`do` ブロックは、複数の `>>=` をわかりやすく書くための構文糖衣です。

## IO モナドでの例

```hs
main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let num = read input :: Int
    print (num * 2)
```

- `IO a` は「副作用を持つ計算の結果 `a`」
- `getLine` は `putStrLn` は `IO` モナドのアクション

## Rust との対比

| Haskell      | Rust                                |
| ------------ | ----------------------------------- |
| `Maybe a`    | `Option<T>`                         |
| `>>=` (bind) | `.and_then()`                       |
| `IO a`       | `Result<T, E>` + `.and_then()` など |
| `do` 記法    | `.and_then()` チェーン              |

## まとめ

- Monad は「モナド値から値を取り出して次のモナド計算を繋げる」仕組み
- 連続処理、失敗伝播、副作用処理の抽象化に使う
- `do` 記法で書くとコードが読みやすくなる
