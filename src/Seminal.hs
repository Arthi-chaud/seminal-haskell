-- | Entrypoint to Seminal
module Seminal (runSeminal, Status(..)) where
import Enumerator.Changes (Change (exec, followups), wrapLoc, wrapChange)
import Compiler.TypeChecker (typecheckModule, TypeCheckStatus(Error, Success))
import Compiler.Parser (parseFile)
import Compiler.Runner (runCompiler)
import GHC (HsModule (hsmodDecls, HsModule), HsDecl, GhcPs, LHsDecl, GenLocated (L), unLoc, ParsedModule (pm_parsed_source, ParsedModule), getLoc)
import Enumerator.Enumerator
import Data.Functor ((<&>))

data Status =
    -- | When the file typechecks without any changes
    Success |
    -- | When an error occurs while parsing the file,
    -- unrelated to typechecking
    InvalidFile String |
    -- | An ordered list of change suggestions
    Changes [Change HsModule]

-- | Run Seminal on a source file.
-- If it returns Nothing, the file typechecks,
-- otherwise, provides an ordered list of change suggestions 
runSeminal :: FilePath -> IO Status
runSeminal filePath = do
    r <- parseFile filePath
    case r of
        Left err -> return $ InvalidFile (show err)
        Right pm -> do
            res <- typecheckPm pm
            case res of
                Compiler.TypeChecker.Success -> return Seminal.Success
                Compiler.TypeChecker.Error _ -> Changes <$> findChanges (typecheckPm . hsModToParsedModule) hsModule
                where
                    hsModule = unLoc $ pm_parsed_source pm
                    typecheckPm = runCompiler . typecheckModule
                    hsModToParsedModule :: HsModule -> ParsedModule
                    hsModToParsedModule m = let
                        (ParsedModule _ src _) = pm
                        srcLoc = getLoc src
                        in pm { pm_parsed_source = L srcLoc m }
-- | Finds the possible changes to apply to a module to make it typecheck
findChanges :: (HsModule -> IO TypeCheckStatus) -> HsModule -> IO [Change HsModule]
findChanges test m = findValidChanges changes
    where
        -- | Hsmodule to changes
        changes = enumerateChangesAtRoot (hsmodDecls m)
            <&> wrapChange (\decls -> m { hsmodDecls = decls })
        -- | runs `evaluate` on all changes
        evaluateAll = mapM evaluate
        -- | Checks if change typechecks
        evaluate change = test (exec change) <&> (\res -> (change, res))
        -- | Takes a list of change, and 
        findValidChanges clist = do
            successfulchanges <- evaluateAll clist <&> filter ((Compiler.TypeChecker.Success ==) . snd) <&> map fst
            case successfulchanges of
                [] -> return successfulchanges
                _ -> (successfulchanges ++) <$> findValidChanges (concatMap followups successfulchanges)
