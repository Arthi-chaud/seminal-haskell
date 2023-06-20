{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Seminal.Compiler.Runner (runCompiler) where
import GHC (Ghc, runGhc, setSessionDynFlags, setTargets, guessTarget, load, LoadHowMuch (LoadAllTargets), Backend (NoBackend), getSessionDynFlags, mkModuleName, ParsedModule, depanal, mgModSummaries, parseModule, GhcException (Panic), DynFlags (maxErrors, extensionFlags))
import GHC.Paths (libdir)
import GHC.Driver.Session
    ( DynFlags(ghcLink, mainFunIs, mainModuleNameIs, backend),
      GhcLink(NoLink) )
import GHC.Plugins (msHsFilePath, throwGhcException)
import Data.List (find)
import Text.Printf (printf)
import Control.Exception (try, SomeException)
import GHC.LanguageExtensions (Extension(PartialTypeSignatures))
import GHC.Data.EnumSet (insert)

type ErrorMessage = String

-- | Setup and run a GHC Session.
-- The file paths are the paths to the source files to load.
-- The action to run takes the list of loaded modules.
-- Upon error (file access, syntax, ...), throws
runCompiler :: forall a . [FilePath] -> ([(FilePath, ParsedModule)] -> Ghc a) -> IO (Either ErrorMessage a)
runCompiler filePaths action = do
    res <- try session :: IO (Either SomeException a)
    return $ case res of
        Left e -> Left $ show e
        Right r -> Right r
    where
        session = runGhc (Just libdir) $ do
            targets <- guessTargets filePaths
            setTargets targets
            flags <- getSessionDynFlags
            setSessionDynFlags (flags {
                mainFunIs = Just "undefined",
                mainModuleNameIs = mkModuleName "Prelude",
                backend = NoBackend,
                ghcLink = NoLink,
                maxErrors = Just 0,
                extensionFlags = insert PartialTypeSignatures (extensionFlags flags)
                })
            _ <- load LoadAllTargets
            modGraph <- depanal [] True
            parseResults <- mapM (\f -> (f,) <$> getModule modGraph f) filePaths
            action parseResults
#if MIN_VERSION_ghc_lib(9,2,8)
        guessTargets = mapM (\t -> guessTarget t Nothing Nothing)
#else
        guessTargets = mapM (`guessTarget` Nothing) -- AKA (\filePath -> guessTarget filePath Nothing)
#endif
        -- Retrieves the module of a file using its paths and the modgraph
        getModule modGraph filePath = case find ((== filePath) . msHsFilePath) (mgModSummaries modGraph) of
            -- Do not worry, this should never happen.
            Nothing -> throwGhcException $ Panic (printf "%s: Could not find module" filePath) 
            Just modsum -> parseModule modsum
        