module Main where

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

main :: IO ()
main = do
    print $ Just 10 >>= \x -> safeDiv x 2 -- Just 5
    print $ Just 10 >>= \x -> safeDiv x 0 -- Nothing
    print $ Nothing >>= \x -> safeDiv x 2 -- Nothing