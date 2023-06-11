{-# LANGUAGE TupleSections #-}
-- | Entrypoint to Seminal
module Seminal (
    runSeminal,
    Status(..),
) where
import Seminal.Options
import Seminal.Change (Change (..), ChangeType(..), getNode, changeGroupToSingle)
import qualified Seminal.Compiler.TypeChecker as TypeChecker
import Seminal.Compiler.Parser (parseFile)
import Seminal.Compiler.Runner (runCompiler)
import GHC (GenLocated (L), unLoc, ParsedModule (pm_parsed_source, ParsedModule), getLoc, HsModule)
import Data.Functor ((<&>))
import Seminal.Enumerator.Modules (enumerateChangesInModule)
import Seminal.Ranker (sortChanges)
import Seminal.Compiler.TypeChecker (ErrorType(..))
import Data.List (find)
import Data.Maybe (fromMaybe)

data Status =
    -- | When the file typechecks without any changes
    Success |
    -- | When an error occurs while parsing the file,
    -- unrelated to typechecking
    InvalidFile String |
    -- | An ordered list of change suggestions,
    -- Along with the original typecheck error
    Changes (String, [Change HsModule])

-- | Run Seminal on a source file.
-- If it returns Nothing, the file typechecks,
-- otherwise, provides an ordered list of change suggestions 
runSeminal :: Options -> FilePath -> IO Status
runSeminal (Options searchMethod) filePath = do
    r <- parseFile filePath
    case r of
        Left err -> return $ InvalidFile (show err)
        Right pm -> do
            res <- typecheckPm pm
            case res of
                TypeChecker.Success -> return Seminal.Success
                TypeChecker.Error err -> case err of
                    (ScopeError _) -> return $ InvalidFile (show err)
                    (TypeCheckError msg) -> Changes . (msg,) . sortChanges <$> changes
                where
                    changes = findChanges searchMethod (typecheckPm . hsModToParsedModule) hsModule
                    hsModule = unLoc $ pm_parsed_source pm
                    typecheckPm = runCompiler . TypeChecker.typecheckModule
                    hsModToParsedModule :: HsModule -> ParsedModule
                    hsModToParsedModule m = let
                        (ParsedModule _ modsrc _) = pm
                        srcLoc = getLoc modsrc
                        in pm { pm_parsed_source = L srcLoc m }

-- | Finds the possible changes to apply to a module Seminal.to make it typecheck.
-- This is the closest thing to the *Searcher* from Seminal (2006, 2007)
findChanges :: SearchMethod -> (HsModule -> IO TypeChecker.TypeCheckStatus) -> HsModule -> IO [Change HsModule]
findChanges method test m = findValidChanges (enumerateChangesInModule m)
    where
        -- | runs `evaluate` on all changes
        evaluateAll = mapM evaluate
        -- | Checks if change typechecks, and make tuple out of result 
        evaluate change = case change of
            Change {} -> test (getNode $ exec change) <&> (change,)
            ChangeGroup {} -> do
                tests <- mapM (\c -> test (getNode c) >>= (\a -> return (c, a))) (execs change)
                let
                    fallbackChange = (head $ execs change, TypeChecker.Error (TypeCheckError "{From Change Group}"))
                    (topChange, status) = fromMaybe fallbackChange (find ((TypeChecker.Success ==) . snd) tests)
                return (changeGroupToSingle change topChange, status)
                    -- )
        -- | If list of changes to evaluate is empty, return
        findValidChanges [] = return []
        -- | Takes a list of change, and 
        findValidChanges clist = do
            successfulchanges <- evaluateAll clist <&> filter ((TypeChecker.Success ==) . snd) <&> map fst
            -- | Stop searching if `method` is lazy and a terminal change is found
            if (method == Lazy) && any ((Terminal ==) . category) successfulchanges
                then return successfulchanges
                else (successfulchanges ++) <$> findValidChanges (concatMap followups successfulchanges)
