# 式（Expression）の表現と評価

- 数値・演算・ブール値・変数などを持つ「式（Expression）」をデータ構造で表す。
- それを「評価（evaluate / eval）」する処理を定義する

これは抽象構文木（AST）を定義して、それを走査して結果を得ることです。

## 構文木の定義

```hs
module Main where

-- Expression with data type
data Expr = Val Int
            | Add Expr Expr
            | Mul Expr Expr
            | Var String
            deriving (Show)

-- Environment mapping memory of variables
type Env = [(String, Int)]

-- 式の評価関数
eval :: Env -> Expr -> Int
eval _ (Val n)       = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Var name)  =
    case lookup name env of
        Just v -> v
        Nothing -> error $ "Unbound variable: " ++ name

-- Test
expr1 = Add (Val 3) (Mul (Val 4) (Val 5))      -- 3 + (4 * 5)
expr2 = Add (Var "x") (Mul (Val 2) (Var "y"))  -- x + (2 * y)

main :: IO ()
main = do
    putStrLn "== Expression Evaluation =="

    putStrLn "\nSimple Expression:"
    print expr1
    print $ eval [] expr1

    putStrLn "\nWith Variables:"
    let env = [("x", 10), ("y", 7)]
    print expr2
    print $ eval env expr2
```

## Description

### `data Expr = ...`

- `Val Int`: 単なる数値
- `Add`, `Mul`: 演算子ノード（式を再帰的に持つ）
- `Var`: 変数名を表す

→ 式を木構造で表現している：まさに再帰型データ構造の応用！

### `eval :: Env -> Expr -> Int`

- 評価には「変数に何が代入されているか」を知る必要があるので、`Env` という環境（連想リスト）を渡します。
- 例えば、`Var "x"` に対して `"x"` を `lookup` して評価

## クエリ言語との関係

これはまさに「式を木で表し、評価する」という、SQL の WHERE 句 や SELECT 句 の評価そのものです。
例：

```sql
SELECT * FROM users WHERE age + 2 * x > 30
```

このような文を、AST に分解して評価していくイメージがこの単元です。

## まとめ

| 概念   | 役割                     |
| ------ | ------------------------ |
| `Expr` | 抽象構文木（式の構造）   |
| `eval` | 評価関数（再帰的に処理） |
| `Env`  | 変数の値を持つ環境       |

## More Details

- [環境（Env）とは？](./docs/DETAILS.md)
