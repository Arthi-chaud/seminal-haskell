module Main (main) where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))

main :: IO ()
main = do
    (filePath:_) <- getArgs
    res <- runSeminal filePath
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile err -> putStrLn err >> exitFailure
        Changes list -> do
            putStrLn "Suggestions:"
            mapM_ print (take 1 $ reverse list)