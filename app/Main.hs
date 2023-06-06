module Main (main) where
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Options.Applicative (execParser)
import Options (optionParser, Options (Options))

main :: IO ()
main = do
    (Options filePath n _ _) <- execParser optionParser
    res <- runSeminal filePath
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile err -> putStrLn err >> exitFailure
        Changes list -> do
            putStrLn "Suggestions:"
            mapM_ print windowedList
            where
                -- Select the n best changes
                windowedList = case n of
                    Nothing -> filteredList
                    Just size -> take size filteredList
                -- Filters the changes by level
                filteredList = list