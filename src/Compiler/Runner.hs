module Compiler.Runner (runCompiler) where
import GHC (Ghc, runGhc, getSessionDynFlags, setSessionDynFlags, DynFlags (..), GhcMode (..), GhcLink (..))
import GHC.Paths (libdir)

-- | Allows runnning function exposed by the compiler's API
runCompiler :: Ghc a -> IO a
runCompiler action = runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags (flags { ghcMode = OneShot, ghcLink = NoLink, mainFunIs = Just "undefined" })
    action
