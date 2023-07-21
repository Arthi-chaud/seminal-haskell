{-# LANGUAGE ScopedTypeVariables #-}
module Seminal.Compiler.Runner (runCompiler) where
import GHC.Paths (libdir)
import "ghc" GHC.Driver.Session
    (
    DynFlags(ghcLink, mainFunIs, mainModuleNameIs, backend),
    GhcLink(NoLink),
    maxErrors, extensionFlags
    )
import "ghc" GHC.Plugins (msHsFilePath, throwGhcException)
import Data.List (find)
import Text.Printf (printf)
import "ghc-lib" GHC.Plugins(liftIO)
import Control.Exception (try, SomeException)
import "ghc" GHC.Data.EnumSet (insert)
import "ghc-boot" GHC.LanguageExtensions.Type (Extension(PartialTypeSignatures))
import "ghc" GHC (runGhc, Ghc, ParsedModule, parseModule, GhcException(Panic), guessTarget, setSessionDynFlags, getSessionDynFlags, setTargets, mkModuleName)
import "ghc" GHC.Driver.Make (load, LoadHowMuch(LoadAllTargets), depanal)
import "ghc" GHC.Unit.Module.Graph (mgModSummaries)
import "ghc" GHC.Driver.Backend(Backend(NoBackend))

type ErrorMessage = String

-- | Setup and run a GHC Session.
-- The file paths are the paths to the source files to load.
-- The action to run takes the list of loaded modules.
-- Upon error (file access, syntax, ...), throws
runCompiler :: forall a . [FilePath] -> ([(FilePath, ParsedModule)] -> Ghc a) -> IO (Either ErrorMessage a)
runCompiler filePaths action = do
    _ <- liftIO $ putStrLn "A1"
    res <- try session :: IO (Either SomeException a)
    return $ case res of
        Left e -> Left $ show e
        Right r -> Right r
    where
        session = runGhc (Just libdir) $ do
            _ <- liftIO $ putStrLn "A2"
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
        guessTargets = mapM (\t -> guessTarget t Nothing)
        -- Retrieves the module of a file using its paths and the modgraph
        getModule modGraph filePath = case find ((== filePath) . msHsFilePath) (mgModSummaries modGraph) of
            -- Do not worry, this should never happen.
            Nothing -> throwGhcException $ Panic (printf "%s: Could not find module" filePath) 
            Just modsum -> parseModule modsum
        