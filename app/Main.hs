module Main (main) where
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Seminal.Options
import qualified Options as Program
import Options.Applicative (execParser)
import Seminal.Change (Change(..), show)
import Control.Monad (unless)

main :: IO ()
main = do
    (Program.Options filePaths n isLazy quiet minLevel) <- execParser Program.optionParser
    let options = Options {
        search = if isLazy then Lazy else Eager 
    }
    res <- runSeminal options $ head filePaths
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile err -> putStrLn err >> exitFailure
        Changes (errMsg, list) -> do
            -- When it is not quiet, print typecheck error message
            -- We add one more newline for format
            unless quiet $ putStrLn (errMsg ++ "\n")
            putStrLn "Suggestions:"
            mapM_ printChange windowedList
            where
                printChange c = putStrLn $ Seminal.Change.show
                    (src c)
                    (exec c)
                    (location c)
                    (message c)
                -- Select the n best changes
                windowedList = case n of
                    Nothing -> filteredList
                    Just size -> take size filteredList
                -- Filters the changes by level
                filteredList = filter ((minLevel <=) . category) list
