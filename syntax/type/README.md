# 型と型注釈（Types and Type Annotations）

Haskell は静的型付けの言語で、型が非常に重要な役割を果たします。
コンパイル時に型が決まり、型によってエラーを未然に防げます。

```hs
-- Specify a type (type annotation)
x :: Int
x = 42

name :: String
name = "Itsuki"

piVal :: Double
piVal = 3.1415

-- Specify types to multiple arguments of a function
add :: Int -> Int -> Int
add a b = a + b
```

## Description

- `x :: Int` のように、`::` で型を明示することができます（省略しても Haskell は推論します）。
- 関数の型注釈では、引数と戻り値の型を「→」でつなげる形になります。
  - `add :: Int -> Int -> Int` は、Int を受け取って Int を返す関数をもう一つ受け取って最終的に Int を返す、という「カリー化された」構造になっています。
- `String` は `Char` のリスト（`[Char]`）のエイリアスです。

## Point

- 型が異なると演算できません：

```hs
ghci> 1 + 2.5
<interactive>:1:1: error: ...
```

解決するには `fromIntegral` などを使って型変換を行います。

- GHCi では `:type` コマンドで型を確認できます：

```hs
ghci> :type "hello"
"hello" :: [Char]

ghci> :type add
add :: Int -> Int -> Int
```
