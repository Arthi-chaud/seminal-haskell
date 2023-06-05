module Main (main) where
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Options.Applicative (execParser)
import Options (optionParser, Options (Options))

main :: IO ()
main = do
    (Options filePath _ _ _) <- execParser optionParser
    res <- runSeminal filePath
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile err -> putStrLn err >> exitFailure
        Changes list -> do
            putStrLn "Suggestions:"
            mapM_ print list