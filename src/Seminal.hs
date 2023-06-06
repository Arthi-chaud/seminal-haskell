-- | Entrypoint to Seminal
module Seminal (
    runSeminal,
    Status(..),
) where
import Seminal.Options
import Seminal.Change (Change (..), ChangeType(..), ChangeDoc (..))
import qualified Seminal.Compiler.TypeChecker as TypeChecker
import Seminal.Compiler.Parser (parseFile)
import Seminal.Compiler.Runner (runCompiler)
import GHC (GenLocated (L), unLoc, ParsedModule (pm_parsed_source, ParsedModule), getLoc, HsModule)
import Data.Functor ((<&>))
import Seminal.Enumerator.Modules (enumerateChangesInModule)
import Seminal.Ranker (sortChanges)

data Status =
    -- | When the file typechecks without any changes
    Success |
    -- | When an error occurs while parsing the file,
    -- unrelated to typechecking
    InvalidFile String |
    -- | An ordered list of change suggestions
    Changes [Change HsModule]
    deriving Show

-- | Run Seminal on a source file.
-- If it returns Nothing, the file typechecks,
-- otherwise, provides an ordered list of change suggestions 
runSeminal :: Options -> FilePath -> IO Status
runSeminal (Options searchMethod)filePath = do
    r <- parseFile filePath
    case r of
        Left err -> return $ InvalidFile (show err)
        Right pm -> do
            res <- typecheckPm pm
            case res of
                TypeChecker.Success -> return Seminal.Success
                TypeChecker.Error _ -> Changes . sortChanges <$> changes
                where
                    changes = findChanges searchMethod (typecheckPm . hsModToParsedModule) hsModule
                    hsModule = unLoc $ pm_parsed_source pm
                    typecheckPm = runCompiler . TypeChecker.typecheckModule
                    hsModToParsedModule :: HsModule -> ParsedModule
                    hsModToParsedModule m = let
                        (ParsedModule _ src _) = pm
                        srcLoc = getLoc src
                        in pm { pm_parsed_source = L srcLoc m }

-- | Finds the possible changes to apply to a module Seminal.to make it typecheck.
-- This is the closest thing to the *Searcher* from Seminal (2006, 2007)
findChanges :: SearchMethod -> (HsModule -> IO TypeChecker.TypeCheckStatus) -> HsModule -> IO [Change HsModule]
findChanges method test m = findValidChanges (enumerateChangesInModule m)
    where
        -- | runs `evaluate` on all changes
        evaluateAll = mapM evaluate
        -- | Checks if change typechecks, and make tuple out of result 
        evaluate change = test (exec change) <&> (\res -> (change, res))
        -- | Takes a list of change, and 
        findValidChanges clist = do
            successfulchanges <- evaluateAll clist <&> filter ((TypeChecker.Success ==) . snd) <&> map fst
            -- | Stop searching if no successful change is found or `method` is lazy and a temrinal change is found
            if null successfulchanges || ((method == Lazy) && any ((Terminal ==) . category . doc) successfulchanges)
                then return successfulchanges
                else (successfulchanges ++) <$> findValidChanges (concatMap followups successfulchanges)