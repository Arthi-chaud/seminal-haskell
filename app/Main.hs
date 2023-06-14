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
    res <- runSeminal options filePaths
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile errs -> mapM_ (putStrLn . snd) errs >> exitFailure
        Changes changes -> mapM_ (printChange quiet n minLevel) changes
        where
            printChange quiet n minLevel (_, errMsg, list) = do
                -- When it is not quiet, print typecheck error message
                -- We add one more newline for format
                unless quiet $ putStrLn (errMsg ++ "\n")
                putStrLn "Suggestions:"
                let 
                    -- Select the n best changes
                    windowedList = case n of
                        Nothing -> filteredList
                        Just size -> take size filteredList
                    -- Filters the changes by level
                    filteredList = filter ((minLevel <=) . category) list
                mapM_ formatChange windowedList
            formatChange c = putStrLn $ Seminal.Change.show
                (src c)
                (exec c)
                (location c)
                (message c)
            