module Main (main) where
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Seminal.Options
import qualified Options as Program
import Options.Applicative (execParser)
import Seminal.Change (ChangeDoc(..), Change (doc))

main :: IO ()
main = do
    (Program.Options filePath n isLazy minLevel) <- execParser Program.optionParser
    let options = Options {
        search = if isLazy then Lazy else Eager 
    }
    res <- runSeminal options filePath
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile err -> putStrLn err >> exitFailure
        Changes (errMsg, list) -> do
            putStrLn errMsg
            putStrLn ""
            putStrLn "Suggestions:"
            mapM_ print windowedList
            where
                -- Select the n best changes
                windowedList = case n of
                    Nothing -> filteredList
                    Just size -> take size filteredList
                -- Filters the changes by level
                filteredList = filter ((minLevel <=) . category . doc) list
                