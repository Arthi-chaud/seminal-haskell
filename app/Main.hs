module Main (main) where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Seminal (runSeminal, Status(..))
import Data.List (intercalate)
import Text.Printf (printf)
import GHC.Plugins (showSDocUnsafe, Outputable (ppr))
import Changes (Change(exec))

main :: IO ()
main = do
    (filePath:_) <- getArgs
    res <- runSeminal filePath
    case res of
        Success -> putStrLn "File Typechecks"
        InvalidFile err -> putStrLn err >> exitFailure
        Changes list -> putStrLn $ printf "Possible changes to apply: [\n%s\n]" (intercalate "\n---\n---\n" ((showSDocUnsafe . ppr . exec) <$> list))