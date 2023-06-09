module Main (main) where
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Seminal.Options
import qualified Options as Program
import Options.Applicative (execParser)
import Seminal.Change (Change(..), showWithMessage)
import Control.Monad (unless, when)
import Text.Printf(printf)
import System.TimeIt

main :: IO ()
main = do
    (Program.Options filePaths n isLazy quiet minLevel countCalls traceCalls time) <- execParser Program.optionParser
    let options = Options {
        search = if isLazy then Lazy else Eager,
        traceTcCalls = traceCalls
    }
    (elapsedTime, res) <- timeItT $ runSeminal options filePaths
    case res of
        Success -> putStrLn "File Typechecks"
        Error errs -> putStrLn errs >> exitFailure
        Result (counts, changes) -> do
            mapM_ (printChange quiet n minLevel) changes
            when time $ printDuration elapsedTime
            when countCalls $ printTypecheckerCallCount counts
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
            -- Duration is in second
            printDuration duration = putStrLn $ printf "Execution Time: %.3f ms" (duration * 1000)
            formatChange c = putStrLn (Seminal.Change.showWithMessage
                (src c)
                (head $ exec c)
                (location c)
                (message c)) >> putStrLn ""
