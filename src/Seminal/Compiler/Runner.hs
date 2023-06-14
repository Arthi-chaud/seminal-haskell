module Seminal.Compiler.Runner (runCompiler) where
import GHC (Ghc, runGhc, setSessionDynFlags, setTargets, guessTarget, load, LoadHowMuch (LoadAllTargets), Backend (NoBackend), getSessionDynFlags, mkModuleName, defaultErrorHandler)
import GHC.Paths (libdir)
import GHC.Driver.Session
import System.IO (stderr, stdout)
import System.IO.Silently
-- | Allows runnning function exposed by the compiler's API.
-- the source file paths are needed to set targets for the session
runCompiler :: [FilePath] -> Ghc a -> IO a
runCompiler filePaths action = hSilence [stderr, stdout] $ runGhc (Just libdir) $ do
    targets <- guessTargets filePaths
    setTargets targets
    flags <- getSessionDynFlags
    setSessionDynFlags (flags {
        mainFunIs = Just "undefined",
        mainModuleNameIs = mkModuleName "Prelude",
        backend = NoBackend,
        ghcLink = NoLink
        })
    _ <- load LoadAllTargets
    action
    where
        guessTargets = mapM (`guessTarget` Nothing) -- AKA (\filePath -> guessTarget filePath Nothing)
