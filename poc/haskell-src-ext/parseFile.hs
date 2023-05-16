import System.Environment
import Language.Haskell.Exts.Parser
import Data.Text
import Text.Printf (printf)
import Language.Haskell.Exts.Syntax
import Data.Aeson
import Language.Haskell.Exts (prettyPrim, prettyPrint)
main = do
    (arg:_) <- getArgs
    contents <- readFile arg
    let r = parseModule contents
    case r of
        ParseOk source  -> do
            -- putStrLn $ printf "Imports: %s" (show imports)
            putStrLn $ printf "Decls: %s" (show decl)
            putStrLn $ prettyPrint source
            where 
                Module _ _ _ _ decl = source
        _ -> putStrLn "Syntax Error"