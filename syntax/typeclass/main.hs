module Main where

-- Typeclass Definition 
class Describable a where
    describe :: a -> String

-- Instance Typeclass Definition
data Animal = Dog | Cat

instance Describable Animal where
    describe Dog = "This is a dog."
    describe Cat = "This is a cat."

-- Apply other types
data Color = Red | Blue

instance Describable Color where
    describe Red = "This is red."
    describe Blue = "This is blue."

-- Common Functions
printDescription :: Describable a => a -> IO ()
printDescription x = putStrLn $ describe x

main :: IO ()
main = do
    putStrLn "== Typeclass Example =="
    printDescription Dog
    printDescription Cat
    printDescription Red
    printDescription Blue