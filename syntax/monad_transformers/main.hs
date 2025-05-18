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
