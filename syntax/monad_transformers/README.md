# モナド変換子（Monad Transformers）とは？

Haskell では、`Maybe`, `IO`, `State` など多用なモナドが存在しますが、それらを「合成」して使いたい場合が多くあります。

## 例：IO と Maybe を組み合わせたい

```hs
getLine :: IO String
readMaybe :: String -> Maybe Int
```

これを組み合わせたい場合：

```hs
do
    str <- getLine           -- IO String
    let mInt = readMaybe str -- Maybe Int
```

ここで「`IO (Maybe Int)`」になりますが、普通の `>>=` や `do` 記法ではつなげません。
ここで登場するのがモナド変換子（`T` のつく型）です！

## モナド変換子の代表例

| 通常モナド | モナド変換子 | 意味                     |
| ---------- | ------------ | ------------------------ |
| `Maybe`    | `MaybeT`     | 「失敗しうる計算」を包む |
| `Either e` | `ExceptT e`  | 「エラーを返す計算」     |
| `State s`  | `StateT s`   | 「状態を扱う計算」       |
| `Reader r` | `ReaderT r`  | 「環境付き計算」         |
| `Writer w` | `WriterT w`  | 「ログを出力する計算」   |

## MaybeT の使い方

`MaybeT` は、モナド `m` に対して「失敗しうる処理」を追加する構造です。

```hs
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

つまり、`MaybeT IO Int` は `IO (Maybe Int)` をラップするモナド

## 使用例：MaybeT IO

```hs
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

getValidNumber :: MaybeT IO Int
getValidNumber = do
    liftIO $ putStrLn "Enter a number:"
    input <- liftIO getLine
    case reads input of
        [(n, "")] -> return n
        _         -> MaybeT $ return Nothing

main :: IO ()
main = do
    result <- runMaybeT getValidNumber
    case result of
        Just n -> putStrLn $ "You entered: " ++ show n
        Nothing -> putStrLn "Invalid input!"
```

- `liftIO` は IO アクションを `MaybeT IO` に「持ち上げる」
- `MaybeT $ return Nothing` は失敗を意味する

## 使い方のポイント

- モナド変換子は「ある機能」を既存のモナドに「合成」する方法
- `MaybeT IO a` のように「重ねる」ことで複雑な文脈を表現できる
- 最後は、`runMaybeT`, `runStateT`, `runReaderT` などで「剥がす」

## 他の例

- `ExceptT String IO`
  - 例外のある IO
- `StateT Int IO`
  - 状態と副作用を持つ計算
- `ReaderT Config IO`
  - 設定を読む IO

## Rust との比較（Option + Result）

```rs
fn get_input() -> Option<String> {
    Some("42".to_string())
}

fn parse(input: &str) -> Option<i32> {
    input.parse().ok()
}

// and_then チェーン
let result = get_input().and_then(|s| parse(&s));
```

これは Haskell では：

```hs
get_input >>= parse
-- MaybeT IO にするなら >>= の代わりに do 記法
```

## モナド変換子の利点

- モナドを自由に合成できる
- 複数の文脈（例：状態管理 + IO + 例外）を自然に表現できる
- クエリ言語など「状態を持つ解析処理」に非常に向いている
