import System.Environment
import GHC.Parser
import GHC.Parser.Lexer
import GHC.Parser.Annotation
import Data.Text
import Text.Printf (printf)
import GHC.Types.SrcLoc
import GHC.Data.StringBuffer
import GHC.Data.FastString (mkFastString)
import GHC (HsModule(hsmodName, hsmodDecls))

runParser :: ParserOpts -> String -> P a -> ParseResult a
runParser opts str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = initParserState opts buffer location
main = do
    (arg:_) <- getArgs
    contents <- readFile arg
    let r = runParser (mkParserOpts undefined undefined  True True True True)contents parseModule
    case r of
        (POk _ a) -> do
            let parsedModule = unLoc a
            putStrLn ""
            print $ hsmodDecls parsedModule
            -- putStrLn $ printf "Decls: %s" (show decl)
        _ -> putStrLn "Syntax Error"