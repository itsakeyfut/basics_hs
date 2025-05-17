# ガード（guards）と `if/then/else`

Haskell では、条件分岐を 2 つの方法で書くことができます：

1. `if ... then ... else`（一般的な条件式）
2. ガード（guard）構文（より Haskell らしい書き方）

```hs
signOf :: Int -> String
signOf x = if x > 0
            then "Positive"
            else if x < 0
                then "Negative"
                else "Zero"
```

```hs
signOf' :: Int -> String
signOf' x
    | x > 0     = "Positive"
    | x < 0     = "Negative"
    | otherwise = "Zero"
```

- `otherwise` は `True` と同期で、デフォルトの条件に使われます。
- これは実施的に `if/then/else` より可読性が高く、拡張もしやすいです。

```hs
bmiTell :: Float -> Float -> String
bmiTell weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    where bmi = weight / height ^ 2
```

## Description

- `where` はガード内で使う補助的な値や変数の定義に便利です。
- `let` との違いは、`where` は定義の末尾に、`let` は先頭や式内に書きます。

## Point

- 複数の条件を使うなら、`if/then/else` よりもガードの方が簡潔で拡張しやすいです。
- `otherwise` を忘れると全ての条件にマッチしない場合にパターン不一致エラーになります。
