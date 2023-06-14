{-# LANGUAGE TupleSections #-}
-- | Provides utilities to parse a file/source code using the compiler's API
module Seminal.Compiler.Parser (
    parseFiles,
    ParsingResult,
    ParsingErrorType (..)
) where
import GHC
    ( parseModule,
      mgModSummaries, depanal, ParsedModule, Ghc )
import Data.List (find)
import Data.Monoid ()
import Control.Monad.Catch (try)
import GHC.Plugins (msHsFilePath, liftIO)
import GHC.Types.SourceError (SourceError)
import System.Directory (getPermissions, Permissions (readable))
import System.Directory.Internal.Prelude (isDoesNotExistError)
import Data.Either (lefts, rights)
import Data.Maybe (fromJust, isJust)
import Control.Arrow (second)

-- | Result of Parsing process
type ParsingResult = Either [(FilePath, ParsingErrorType)] [(FilePath, ParsedModule)]

data ParsingErrorType =
    -- | Permission to read the file denied
    PermissionDenied |
    -- | The file was not found
    FileNotFound |
    -- | There was an error syntax in the source code
    SyntaxError |
    -- | An unknown error occured
    -- | Comes with an error messag
    UnknownError String
    deriving (Show, Eq)

-- | Parse files and retrieve their modules
parseFiles :: [FilePath] -> Ghc ParsingResult
parseFiles filePaths = do
    checks <- mapM (\f -> (f, ) <$> liftIO (checkFileAvailability f)) filePaths
    case filter (isJust . snd) checks of
        [] -> parseFiles' filePaths
        -- If there is at least one parsing Error 
        errors -> return $ Left (second fromJust <$> errors)

-- | Checks if a file exists and is readable
checkFileAvailability :: FilePath -> IO (Maybe ParsingErrorType)
checkFileAvailability filePath = do
    permRes <- try (getPermissions filePath) :: IO (Either IOError Permissions)
    case permRes of
        Left err -> if isDoesNotExistError err
            then return (Just FileNotFound)
            else return (Just $ UnknownError "")
        Right perms -> if readable perms
            then return Nothing
            else return $ Just PermissionDenied

-- | Invoke GHC's typechecker for all files at once.
-- Does not handle related to permissions/existence of file 
parseFiles' :: [FilePath] -> Ghc ParsingResult
parseFiles' filePaths = do
        modGraph <- depanal [] True
        parseResults <- mapM (getModule modGraph) filePaths
        return $ case lefts parseResults of
            [] -> Right (rights parseResults)
            -- If there is 1+ error, return them
            errs -> Left errs
        where
        -- Retrieves the module of a file using its paths and the modgraph
        getModule modGraph filePath = case find ((== filePath) . msHsFilePath) (mgModSummaries modGraph) of
            Nothing -> return $ Left (filePath, UnknownError "Could not find module")
            Just modsum -> do
                res <- try (parseModule modsum) :: Ghc (Either SourceError ParsedModule)
                return $ case res of
                    Right pm -> Right (filePath, pm)
                    _ -> Left (filePath, SyntaxError)