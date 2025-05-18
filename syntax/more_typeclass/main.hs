module Main where

data Color = Red | Green | Blue

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

main :: IO ()
main = do
    putStrLn "Comparing Colors:"
    print (Red == Red)   -- True
    print (Red == Green) -- False

    print Red -- "Red"
    print [Red, Blue] -- ["Red", "Blue"]