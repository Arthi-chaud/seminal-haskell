module Main (main) where
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Seminal.Options
import qualified Options as Program
import Options.Applicative (execParser)
import Seminal.Change (Change(..), show)
import Control.Monad (unless, when)
import Text.Printf(printf)

main :: IO ()
main = do
    (Program.Options filePaths n isLazy quiet minLevel countCalls) <- execParser Program.optionParser
    let options = Options {
        search = if isLazy then Lazy else Eager
    }
    res <- runSeminal options filePaths
    case res of
        Success -> putStrLn "File Typechecks"
        Error errs -> putStrLn errs >> exitFailure
        Result (counts, changes) -> do
            when countCalls $ printTypecheckerCallCount counts
            mapM_ (printChange quiet n minLevel) changes
        where
            printTypecheckerCallCount c = putStrLn $ printf "Typechecker call count: %d" c
            printChange quiet n minLevel (filePath, errMsg, list) = do
                -- When it is not quiet, print typecheck error message
                -- We add one more newline for format
                unless quiet $ putStrLn (filePath ++ " - " ++ errMsg ++ "\n")
                putStrLn "Suggestions:"
                let
                    -- Select the n best changes
                    windowedList = case n of
                        Nothing -> filteredList
                        Just size -> take size filteredList
                    -- Filters the changes by level
                    filteredList = filter ((minLevel <=) . category) list
                mapM_ formatChange windowedList
            formatChange c = putStrLn (Seminal.Change.show
                (src c)
                (head $ exec c)
                (location c)
                (message c)) >> putStrLn ""
