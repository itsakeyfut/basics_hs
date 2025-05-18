# リストとリスト内包表記（List and List Comprehensions）

Haskell ではリストがとても重要で、標準ライブラリもリスト操作を中心に設計されています。

## Create lists and how to use it

```hs
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

-- list concatenation
combined = [1, 2] ++ [3, 4]

-- cons (push front)
moreNumbers = 0 : numbers

-- pop
first = head numbers -- 1
rest  = tail numbers -- [2, 3, 4, 5]
lastN = last numbers -- 5
```

## List comprehensions

```hs
-- numbers powered by 2 list in range of 1 - 10
squares = [x * x | x <- [1..10]]

-- pop even numbers
evens = [x | x <- [1..20], even x]

-- filter chars
noVowels = [c | c <- "Hello World", not (c `elem` "aeiouAEIOU")]
```

## Nested list comprehensions

```hs
-- nested list behaving like nested loop
pairs = [(x, y) | x <- [1, 2], y <- ['a', 'b']]
-- => [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]
```

## Description

- `[x | x <- xs, 条件]` は「x を xs から取り出して条件を満たすものを生成する」形。
- `x <- [1..10]` はリストから要素を取り出すジェネレーターのような役割
- `++` はリストの結合、`:` は cons（先頭に要素追加）
- `elem` は「リストに要素が含まれているか」を判定します。

## Point

- Haskell ではリストはリンクリスト構造なので、先頭に要素を追加（`:`）する操作は高速ですが、末尾追加や `++` はコストが高いです。
- リスト内包表記はデータ加工・フィルター処理に非常に便利です。
