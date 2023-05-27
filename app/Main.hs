module Main (main) where
import Compiler.Parser (parseFile)
import Compiler.TypeChecker (typecheckModule, TypeCheckStatus (..))
import System.Environment (getArgs)
import Compiler.Runner (runCompiler)

main :: IO ()
main = do
    (filePath:_) <- getArgs
    r <- parseFile filePath
    case r of
        Left err -> print err
        Right m -> do
            res <- runCompiler $ typecheckModule m
            putStrLn $ case res of 
                Success -> "File Typechecks"
                Error _ -> "Typecheck failed"
