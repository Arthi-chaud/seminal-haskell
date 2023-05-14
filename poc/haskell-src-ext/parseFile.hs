import System.Environment
import Language.Haskell.Exts.Parser
import Data.Text
import Text.Printf (printf)
import Language.Haskell.Exts.Syntax
import Data.Aeson
main = do
    (arg:_) <- getArgs
    contents <- readFile arg
    let r = parseModule contents
    case r of
        ParseOk (Module _ _ _ imports decl)  -> do
            putStrLn $ printf "Imports: %s" (show imports)
            putStrLn $ printf "Decls: %s" (show decl)
        _ -> putStrLn "Syntax Error"