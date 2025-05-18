# 再帰型データ構造（木構造など）と再帰関数

これは抽象構文木（AST）やクエリツリーなど、構文解析と評価処理に必須の技術です。

## 再帰型データ構造の定義

再帰的なデータ型の代表例が 二分木（Binary Tree）です。

```hs
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)
```

- `Tree a` は型引数 `a` を持つジェネリックな木構造
- 空の木（`Empty`）か、値 `a` と左右のサブツリー（`Node`）で構成。

```hs
module Main where

-- Recursive Tree Data Structure
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

-- Convert elements into List in-order
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

-- Compute Tree Height
treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- Test tree
testTree :: Tree Int
testTree = Node 10
            (Node 5 Empty (Node 7 Empty Empty))
            (Node 15 Empty Empty)

main :: IO ()
main = do
    putStrLn "== Binary Tree =="
    print testTree

    putStrLn "\n== In-order Traversal =="
    print $ inOrder testTree

    putStrLn "\n== Tree Height =="
    print $ treeHeight testTree
```

## Description

`data Tree a = ...`

- 再帰的定義により、任意に深い木を表現できます。
- Haskell では、リストも実は同様に再帰的データ型です：

```hs
data List a = Nil | Cons a (List a)
```

### 再帰関数の考え方

- Empty がベースケース
- `Node x l r` で再帰的に左右の部分木を処理。

## クエリ言語との関係

- 構文木（AST）は、このような再帰的構造になります。
- たとえば `SELECT name FROM users WHERE age > 30` という構文も木に展開できます。
- 今後、式（Expression）や条件（Predicate）なども再帰構造で定義します。

## まとめ

| 概念             | 用途                         |
| ---------------- | ---------------------------- |
| 再帰型データ構造 | 木構造・リスト・AST 表現など |
| パターンマッチ   | 構造を解体・処理に必要       |
| 再帰関数         | データ構造を操作する基本技法 |

## More Details

- [再帰型データ構造とは？](./docs/DETAILS.md)
