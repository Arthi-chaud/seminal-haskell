module Compiler.Runner (runCompiler) where
import GHC (Ghc, runGhc, getSessionDynFlags, setSessionDynFlags)
import GHC.Paths (libdir)

-- | Allows runnning function exposed by the compiler's API
runCompiler :: Ghc a -> IO a
runCompiler action = runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags
    action
