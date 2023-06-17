{-# LANGUAGE TupleSections #-}
-- | Entrypoint to Seminal
module Seminal (
    runSeminal,
    Status(..),
) where
import Seminal.Options (Options(Options), SearchMethod(Lazy))
import Seminal.Change (Change (..), ChangeType(..), getNode, changeGroupToSingle)
import qualified Seminal.Compiler.TypeChecker as TypeChecker
import Seminal.Compiler.Runner (runCompiler)
import GHC (GenLocated (L), unLoc, ParsedModule (pm_parsed_source, ParsedModule), getLoc, HsModule, Ghc)
import Data.Functor ((<&>))
import Seminal.Enumerator.Modules (enumerateChangesInModule)
import Seminal.Compiler.TypeChecker (ErrorType(..), isScopeError, getTypeCheckError)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Prelude hiding (mod)
import Data.Tuple.HT (thd3)
import Seminal.Ranker (sortChanges)

type ErrorMessage = String

data Status =
    -- | When the file typechecks without any changes
    Success |
    -- | When an error occurs during the GHC session,
    -- unrelated to typechecking
    Error ErrorMessage |
    -- | For each file that does not typecheck, an ordered list of change suggestions,
    -- Along with the original typecheck error
    Changes [(FilePath, ErrorMessage, [Change HsModule])]

-- | Run Seminal on a source file.
-- If it returns Nothing, the file typechecks,
-- otherwise, provides an ordered list of change suggestions 
runSeminal :: Options -> [FilePath] -> IO Status
runSeminal (Options searchMethod) filePaths = either Error id <$> ghcAction
    where
        ghcAction = runCompiler filePaths $ \filesAndModules -> do
            res <- mapM (\(f, m) -> (f,m,) <$> typecheckPm m) filesAndModules
            case mapMaybe (\(f, mod, status) -> (f,mod,) <$> getTypeCheckError status) res of
                -- If nothing is unsuccessful <-> If all typechecks
                [] -> return Success
                errs -> case filter (isScopeError . thd3) errs of
                    [] -> Changes <$> mapM (\(f, m, err) -> (f, show err, ) . sortChanges <$> changes m) errs
                    scopeErrors -> return $ Error $ concatMap (\(f, _, err) -> f ++ " - " ++ show err) scopeErrors
        changes pm = findChanges searchMethod (typecheckPm . wrapHsModule pm) (hsModule pm)
        hsModule = unLoc . pm_parsed_source
        typecheckPm = TypeChecker.typecheckModule
        wrapHsModule :: ParsedModule -> HsModule -> ParsedModule
        wrapHsModule pm m = let
            (ParsedModule _ modsrc _) = pm
            srcLoc = getLoc modsrc
            in pm { pm_parsed_source = L srcLoc m }

-- | Finds the possible changes to apply to a module Seminal.to make it typecheck.
-- This is the closest thing to the *Searcher* from Seminal (2006, 2007)
findChanges :: SearchMethod -> (HsModule -> Ghc TypeChecker.TypeCheckStatus) -> HsModule -> Ghc [Change HsModule]
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
        -- | If list of changes to evaluate is empty, return
        findValidChanges [] = return []
        -- | Takes a list of change, and 
        findValidChanges clist = do
            successfulchanges <- evaluateAll clist <&> filter ((TypeChecker.Success ==) . snd) <&> map fst
            -- | Stop searching if `method` is lazy and a terminal change is found
            if (method == Lazy) && any ((Terminal ==) . category) successfulchanges
                then return successfulchanges
                else (successfulchanges ++) <$> findValidChanges (concatMap followups successfulchanges)
