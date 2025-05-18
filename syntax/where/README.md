# `let` と `where` の使い分け（局所定義）

Haskell では式の中で局所的な定義を行うために、`let` と `where` という構文が用意されています。
どちらも一時的な変数や関数の定義に使えますが、使い方と位置が異なります。

## let statement

```hs
calcArea :: Float -> Float
caclArea r =
    let piVal = 3.1415
    in piVal * r * r
```

- `let <定義> in <式>` の形をとります。
- 関数内だけでなく、GHCi でも使えます。

```hs
ghci> let x = 2 in x + 3
5
```

## where clause

```hs
calcArea' :: Float -> Float
caclArea' r = piVal * r * r
    where piVal = 3.1415
```

- `where` は関数本体の後に書いて、補助的な値や関数を定義します。
- ガーとと組み合わせることが多いです：

```hs
bmiTell :: Float -> Float -> String
bmiTell weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | otherwise   = "Overweight"
    where bmi = weight / height ^ 2
```

## `let` vs `where` の違い

| 特徴     | `let`                         | `where`                      |
| -------- | ----------------------------- | ---------------------------- |
| 書く場所 | **式の中**                    | **関数定義の末尾**           |
| スコープ | **ローカルな式全体**          | 定義された関数の式全体       |
| 使う頻度 | GHCi や do 記法で多い         | ガードや関数定義と相性が良い |
| 複数定義 | `let x = ...; y = ... in ...` | 複数行でも読みやすい         |

## 応用例：`let` で複数変数を同時に定義

```hs
hypotenuse :: Float -> Float -> Float
hypotenuse a b =
    let a2 = a * a
        b2 = b * b
    in sqrt (a2 + b2)
```
