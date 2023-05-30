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
        Changes _ -> putStrLn "Possible changes to apply: []"