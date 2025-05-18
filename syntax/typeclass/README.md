# 型クラス（Typeclass）とその実装

Haskell の「型クラス」は、型に共通の振る舞い（インターフェース）を与える仕組みです。
OOP の「インターフェース」や「トレイト（Rust）」と似ていますが、より抽象的で柔軟な機能を提供します。

## 型クラスの定義と使い方

```hs
module Main where

-- Typeclass Definition
class Describable a where
    describe :: a -> String

-- Instance Typeclass Definition
data Animal = Dog | Cat

instance Describable Animal where
    describe Dog = "This is a dog."
    describe Cat = "This is a cat."

-- Apply other types
data Color = Red | Blue

instance Describable Color where
    describe Red = "This is red."
    describe Blue = "This is blue."

-- Common Functions
printDescription :: Describable a => a -> IO ()
printDescription x = putStrLn $ describe x

main :: IO ()
main = do
    putStrLn "== Typeclass Example =="
    printDescription Dog
    printDescription Cat
    printDescription Red
    printDescription Blue
```

## Description

### `class Describable a where`

- `Describable` という型クラスを定義。
- これは「describe 関数を持つ型」というインターフェースのようなもの

### `instance Describable Animal where`

- `Animal` 型が `Describable` であることを宣言。
- `describe` 関数を `Animal` に対して定義する。

### 関数に制約をつける

```hs
printDescription :: Describable a => a -> IO ()
```

- この関数は「Describable 型の値」であれば何でも処理できる。

## クエリ言語にどう関係するか？

クエリ言語では、以下のように多用な値に共通の「評価」処理を施したいことがあります：

- `Int`, `Bool`, `String`, `Column`, `Expression` などに対して `eval` を定義したい
- 型クラスを使うことで汎用的で拡張性のある評価関数が書けます。

## まとめ

| 要素       | 説明                                       |
| ---------- | ------------------------------------------ |
| `class`    | 型クラス定義（インターフェースの宣言）     |
| `instance` | 型に対する実装（インターフェースの具体化） |
| `=>`       | 制約（「Describable であること」が条件）   |

## コードで覚えるべき最小テンプレート

```hs
class Foo a where
    bar :: a -> String

instance Foo MyType where
    bar _ = "hello"
```
