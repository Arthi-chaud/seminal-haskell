-- Inspired by https://stackoverflow.com/questions/2242986/need-a-tutorial-for-using-ghc-to-parse-and-typecheck-haskell
-- Which itself seems to come from https://wiki.haskell.org/GHC/As_a_library
import GHC
import GHC.Paths (libdir)
import System.Environment (getArgs)
import Data.List (singleton)
import GHC.Utils.Outputable

main = do
    runGhc ghcFolder action
    -- Error Handling to do by hand:
    -- if compilation fails, throws
    where
        ghcFolder = Just libdir
        action = do
            -- In Shorts: the configuration to run GHC
            -- https://hackage.haskell.org/package/ghc-9.2.7/docs/GHC-Driver-Session.html#t:DynFlags

            dynamicFlags <- getSessionDynFlags
            -- Not need to re-apply them, if no changes are needed
            setSessionDynFlags dynamicFlags

            -- Transform File Path into 'input' for GHC
            -- Do not know what the Nothing (Maybe Phase) is
            target <- guessTarget "../assets/test.hs" Nothing
            setTargets $ singleton target
            -- Attempt to load programm
            -- Get targets from parameters sent to 'setTargets'
            load LoadAllTargets
            -- Get summary of module using its name
            -- mkModuleName raises String to ModuleName
            -- TODO find module name
            modsum <- getModSummary $ mkModuleName "Main"
            -- Get AST of module
            p <- parseModule modsum
            -- Typecheck Module
            t <- typecheckModule p
            -- Desugar Module
            d <- desugarModule t
            -- Compile Module
            -- Might create .hi files
            l <- loadModule d
            -- Might be names of resolved symbols 
            n <- getNamesInScope
            -- https://hackage.haskell.org/package/ghc-9.2.7/docs/GHC-Unit-Module-ModGuts.html#t:ModGuts
            -- 'Digest' the compiled module
            c <- return $ coreModule d
            -- Get the module dependency graph
            g <- getModuleGraph
            -- print $ showModule modsum
            return (parsedSource d,"/n-----/n",  typecheckedSource d)