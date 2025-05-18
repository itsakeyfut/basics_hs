# `Eq` 型クラス

「等しいかどうか」を比較できる型に共通のインターフェースを定義する。

```hs
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not  (x == y)
```

これは「`==` を実装したら `/=` は自動で定義される」というデフォルト実装のある例です。
Rust の `trait` における default 実装と似ています。

```hs
module Main where

data Color = Red | Green | Blue

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

main :: IO ()
main = do
    putStrLn "Comparing Colors:"
    print (Red == Red)   -- True
    print (Red == Green) -- False

    print Red -- "Red"
    print [Red, Blue] -- ["Red", "Blue"]
```

## Rust との比較

| 概念             | Haskell                 | Rust                           |
| ---------------- | ----------------------- | ------------------------------ |
| 抽象的な振る舞い | `class Eq a where ...`  | `trait Eq { fn eq(...) }`      |
| 実装             | `instance Eq Color`     | `impl Eq for Color`            |
| デフォルト実装   | `x /= y = not (x == y)` | `fn ne(...) { !self.eq(...) }` |
| 呼び出し         | `x == y`                | `x == y`（`PartialEq`）        |

## 型クラスの重要性

- 抽象的な演算を定義できる
- 汎用的な関数を型クラス制約で書ける
- Rust の `trait bound` やジェネリクスと密接に対応
