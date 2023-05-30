-- | Entrypoint to Seminal
module Seminal (runSeminal, Status(..)) where
import Changes (Change)
import Compiler.TypeChecker (typecheckModule, TypeCheckStatus(Error, Success))
import Compiler.Parser (parseFile)
import Compiler.Runner (runCompiler)

data Status =
    -- | When the file typechecks without any changes
    Success |
    -- | When an error occurs while parsing the file,
    -- unrelated to typechecking
    InvalidFile String |
    -- | An ordered list of change suggestions
    Changes [Change]

-- | Run Seminal on a source file.
-- If it returns Nothing, the file typechecks,
-- otherwise, provides an ordered list of change suggestions 
runSeminal :: FilePath -> IO Status
runSeminal filePath = do
    r <- parseFile filePath
    case r of
        Left err -> return $ InvalidFile (show err)
        Right m -> do
            res <- runCompiler $ typecheckModule m
            return $ case res of 
                Compiler.TypeChecker.Success -> Seminal.Success
                Compiler.TypeChecker.Error _ -> Changes []