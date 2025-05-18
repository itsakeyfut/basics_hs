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