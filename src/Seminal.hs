{-# LANGUAGE TupleSections #-}
-- | Entrypoint to Seminal
module Seminal (
    runSeminal,
    Status(..),
) where
import Seminal.Options (Options(Options), SearchMethod(Lazy))
import Seminal.Change (Change (..), ChangeType(..), getNode, show)
import qualified Seminal.Compiler.TypeChecker as TypeChecker(typecheckModule, TypeCheckStatus(..))
import Seminal.Compiler.Runner (runCompiler)
import GHC (GenLocated (L), unLoc, ParsedModule (pm_parsed_source, ParsedModule), getLoc, HsModule, Ghc, GhcPs)
import Seminal.Enumerator.Modules (enumerateChangesInModule)
import Seminal.Compiler.TypeChecker (ErrorType(..), isScopeError, getTypeCheckError)
import Data.Maybe (mapMaybe)
import Prelude hiding (mod)
import Data.Tuple.HT (thd3)
import Seminal.Ranker (sortChanges)
import Data.Bifunctor (second)
import GHC.Plugins (liftIO)
import Control.Monad (when)

type ErrorMessage = String

data Status =
    -- | When the file typechecks without any changes
    Success |
    -- | When an error occurs during the GHC session,
    -- unrelated to typechecking
    Error ErrorMessage |
    -- | For each file that does not typecheck, an ordered list of change suggestions,
    -- Along with the original typecheck error
    Result (
        -- | The number of calls to the typechecker
        Int,
        -- | The valid changes found
        [(FilePath, ErrorMessage, [Change (HsModule GhcPs)])]
    )


-- | Run Seminal on a source file.
-- If it returns Nothing, the file typechecks,
-- otherwise, provides an ordered list of change suggestions 
runSeminal :: Options -> [FilePath] -> IO Status
runSeminal (Options searchMethod traceCalls) filePaths = either Error id <$> ghcAction
    where
        ghcAction = runCompiler filePaths $ \filesAndModules -> do
            res <- mapM (\(f, m) -> (f,m,) <$> typecheckPm m) filesAndModules
            case mapMaybe (\(f, mod, status) -> (f,mod,) <$> getTypeCheckError status) res of
                -- If nothing is unsuccessful <-> If all typechecks
                [] -> return Success
                errs -> case filter (isScopeError . thd3) errs of
                    [] -> do
                        foundChanges <- mapM (\(f, m, err) -> (\(n, c) -> (n, (f, sortChanges c, err))) <$> changes m) errs
                        let totalCall = sum $ fst <$> foundChanges
                            formattedChanges = (\(f, m, err) -> (f, Prelude.show err, m)) . snd <$> foundChanges
                        return $ Result (totalCall, formattedChanges)
                    scopeErrors -> return $ Error $ concatMap (\(f, _, err) -> f ++ " - " ++ Prelude.show err) scopeErrors
        changes pm = findChanges searchMethod (evaluateChange pm traceCalls) (hsModule pm)
        hsModule = unLoc . pm_parsed_source
        typecheckPm = TypeChecker.typecheckModule

-- | Finds the possible changes to apply to a module Seminal.to make it typecheck.
-- | Returns the number of calls to the typechecker along with the found changes
-- This is the closest thing to the *Searcher* from Seminal (2006, 2007)
findChanges :: SearchMethod -> (Change (HsModule GhcPs) -> Ghc (Change (HsModule GhcPs), Int, TypeChecker.TypeCheckStatus)) -> HsModule GhcPs -> Ghc (Int, [Change (HsModule GhcPs)])
findChanges method test m = findValidChanges 0 (enumerateChangesInModule m)
    where
        -- | runs `evaluate` on all changes
        evaluateAll = mapM test
        -- | If list of changes to evaluate is empty, return
        findValidChanges n [] = return (n, [])
        -- | Takes a list of change, and 
        findValidChanges n clist = do
            changesWithTcCalls <- evaluateAll clist
            -- | Compute the number of changes to test using the 'exec' lists length
            let execsCounts = sum ((\(_, c, _) -> c) <$> changesWithTcCalls)
            -- | Predict the number of calls to the typechecker
                tcCalls = n + execsCounts
                changes = (\(a, _ , b) -> (a, b)) <$> changesWithTcCalls
                successfulchanges = fst <$> filter ((TypeChecker.Success ==) . snd) changes
            -- | Stop searching if `method` is lazy and a terminal change is found
            if (method == Lazy) && any ((Terminal ==) . category) successfulchanges
                then return (tcCalls, successfulchanges)
                else second (successfulchanges ++) <$> findValidChanges tcCalls (concatMap followups successfulchanges)

-- | Calls typechecker on Change.
-- Will return the change with the number of calls to the TC and the typecheck status.
-- The `exec` field will only contain one element
evaluateChange :: ParsedModule -> Bool -> Change (HsModule GhcPs) -> Ghc (Change (HsModule GhcPs), Int, TypeChecker.TypeCheckStatus)
evaluateChange pm traceCall change = case exec change of
    [] -> return (change, 0, TypeChecker.Error $ TypeCheckError "{From Change Group}" )
    (a:b) -> do
        statusA <- if traceCall then traceTcCall a else callTypecheckerOnExec a
        case statusA of
            TypeChecker.Success -> return (change { exec = [a] }, 1, statusA)
            _ -> (\(m, count, s) -> (m, count + 1, s)) <$> evaluateChange pm traceCall (change { exec = b })
    where
        traceTcCall e = do
            _ <- liftIO $ when traceCall $ do
                putStrLn (Seminal.Change.show (src change) e (location change))
                putStr "Status: "
            status <- callTypecheckerOnExec e
            _ <- liftIO $ putStrLn $ case status of
                TypeChecker.Success -> "Success\n"
                _ -> "Fail\n"
            return status
        callTypecheckerOnExec s = TypeChecker.typecheckModule (wrapHsModule pm (getNode s))


-- | Util function to apply a changed HsModule onto a ParsedModule
wrapHsModule :: ParsedModule -> HsModule GhcPs -> ParsedModule
wrapHsModule pm m = let
    (ParsedModule _ modsrc _) = pm
    srcLoc = getLoc modsrc
    in pm { pm_parsed_source = L srcLoc m }