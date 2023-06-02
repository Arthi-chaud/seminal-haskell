module Main (main) where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Data.List (intercalate)
import Text.Printf (printf)

main :: IO ()
main = do
    (filePath:_) <- getArgs
    res <- runSeminal filePath
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile err -> putStrLn err >> exitFailure
        Changes list -> putStrLn $ printf "Possible changes to apply: %s" (intercalate "\n\n---\n" (show <$> list))