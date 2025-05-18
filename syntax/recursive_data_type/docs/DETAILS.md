# 再帰型データ構造とは？

## 「木構造」の定義をもう一度：

```hs
data Tree a = Empty
            | Node a (Tree a) (Tree a)
```

この `Tree a` の定義のポイントは、

> 「Tree a の定義の中に、Tree a が含まれている」

ということです。

## 再帰の正体

Haskell のこの定義は、自分自身（同じ型）を部品として再び使っている構造を意味しています。

```hs
Node a (Tree a) (Tree a)
-- Node というコンストラクタは、左と右にまた Tree a を持っている！
```

これはつまり、木構造の中に、また木構造があるということ。

## 具体例で見てみる

```hs
testTree = Node 10
            (Node 5 Empty (Node 7 Empty Empty))
            (Node 15 Empty Empty)
```

この構造を図にすると：

```
        10
       /  \
     5     15
      \
       7
```

これは以下のように Node の中にまた Node がいる = 再帰している状態です。

## 再帰の階層構造

```hs
Node 10
    (Node 5
        Empty
        (Node 7 Empty Empty))
    (Node 15 Empty Empty)
```

このデータはこう展開されます：

- Node 10 は：
  - 左に `Node 5` を持っており、
    - その `Node 5` は右に `Node 7` を持っている
  - 右に `Node 15` を持っている

→ つまり、「木の中にまた木が...」という再帰的な構造です。

## 再帰関数が合う理由

再帰型には再帰関数がよく合います。

```hs
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)
```

この関数も：

- 自分自身（treeHeight）を、また自分の中で呼んでいる（再帰）
- `Node` の中の `Tree a` を処理するために、また `treeHeight` を呼び出す

## まとめ

| 構造        | 再帰の場所                                 |
| ----------- | ------------------------------------------ |
| `data Tree` | 自分自身を含む定義（Tree a の中に Tree a） |
| `testTree`  | 木のノードの中にまた木が入っている         |
| 関数定義    | 自分の中で自分を呼ぶ（再帰関数）           |
