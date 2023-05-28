module Compiler.Runner (runCompiler) where
import GHC (Ghc, runGhc, getSessionDynFlags, setSessionDynFlags)
import GHC.Paths (libdir)

-- | Runs compiler (or function exposed by its API)
runCompiler :: Ghc a -> IO a
runCompiler action = runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags
    action
