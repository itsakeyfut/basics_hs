# Functor

構造を保ったまま関数を適用する

## 基本定義

```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

- 目的：`f a` という「何かしらの構造（コンテナ）」に入った値に関数 `(a -> b)` を適用して、`f b` にする。
- Rust の `.map()` に相当（特に `Option`, `Result`, `Iterator` に対して）

## 直感的な例：リスト

```hs
fmap (+1) [1, 2, 3] -- => [2, 3, 4]
```

- `fmap` はリストの各要素に関数を適用します。
- リスト自体（構造）は保たれます。

```hs
main :: IO ()
main = do
    print $ fmap (*2) [1, 2, 3] -- [2, 4, 6]
```

## Maybe 型の例（`Just`, `Nothing`）

```hs
fmap (+1) (Just 3) -- => Just 4
fmap (+1) Nothing  -- => Nothing
```

```hs
main :: IO ()
main = do
    print $ fmap (+1) (Just 5) -- 6
    print $ fmap (+1) Nothing  -- Nothing
```

### 裏側でどう動いている？

```hs
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing = Nothing
```

- 値があるとき（`Just x`）だけ関数を適用。
- `Nothing` は「何もない」ので何も適用しない。

## なぜ「Functor」が重要なのか？

- コンテナっぽい構造（`Maybe`, `List`, `IO`, `Either` など）に統一的に関数を適用できる。
- 値に触れずに構造の中で変換を行える。
- Rust の `.map()` に慣れていれば直感的に扱える。

## 自作データ型への Functor インスタンス定義

```hs
data Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box x) = Box (f x)
```

```hs
main :: IO ()
main = print $ fmap (+3) (Box 10) -- Box 13
```

## 注意：Functor 法則（守るべきルール）

1. 恒等性
   `fmap id == id`

2. 合成性
   `fmap (f . g) == fmap f . fmap g`

この 2 つを満たすことで、Functor としての「信頼性」が保たれます。

## Rust との比較

```rs
Some(3).map(|x| x + 1); // => Some(4)
None::<i32>.map(|x| x + 1); // => None
```

→ Haskell の `fmap (+1) (Just 3)` と同じ！

## まとめ

- `fmap` は「構造を壊さずに関数適用」する仕組み
- 「`Maybe` でも `[]` でも `IO` でも」同じように扱える
- Rust の `.map()` を意識しておくと理解がスムーズ
