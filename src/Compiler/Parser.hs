-- Provides utilities to parse a file/source code using the compiler's API
module Compiler.Parser (
    parseFile,
    ParsingResult,
    ParsingErrorType (..)
) where
import GHC
    ( guessTarget,
      parseModule,
      setTargets,
      mgModSummaries, depanal, ParsedModule, Ghc )
import Data.List ( find )
import Data.Monoid ()
import Control.Monad.Catch (try)
import GHC.Plugins (msHsFilePath)
import GHC.Types.SourceError (SourceError)
import System.Directory (getPermissions, Permissions (readable))
import System.Directory.Internal.Prelude (isDoesNotExistError)
import Compiler.Runner (runCompiler)

-- Result of Parsing process
type ParsingResult = Either ParsingErrorType ParsedModule

data ParsingErrorType =
    -- Permission to read the file denied
    PermissionDenied |
    -- The file was not found
    FileNotFound |
    -- There was an error syntax in the source code
    SyntaxError |
    -- An unknown error occured
    -- Comes with an error messag
    UnknownError String
    deriving Show

-- Parse a file, retrieve its module
parseFile :: FilePath -> IO ParsingResult
parseFile filePath = do
    permRes <- try (getPermissions filePath) :: IO (Either IOError Permissions)
    case permRes of
        Left err -> if isDoesNotExistError err
            then return (Left FileNotFound)
            else return (Left $ UnknownError "")
        Right perms -> if readable perms
            then parseFile' filePath
            else return $ Left PermissionDenied

-- Parse File, does not handle related to permissions/existence of file 
parseFile' :: FilePath -> IO ParsingResult
parseFile' filePath = runCompiler action
    where
        action = do
            target <- guessTarget filePath Nothing
            setTargets [target]
            modGraph <- depanal [] True
            case find ((== filePath) . msHsFilePath) (mgModSummaries modGraph) of
                Nothing -> return $ Left $ UnknownError "Could not find module" 
                Just modsum -> do
                    res <- try (parseModule modsum) :: Ghc (Either SourceError ParsedModule)
                    return $ case res of
                        Left _ -> Left SyntaxError
                        Right p -> Right p