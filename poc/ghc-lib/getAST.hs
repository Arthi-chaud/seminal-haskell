module Main (main, getAST) where

import GHC.Paths
import Text.Printf
import GHC.Plugins
import System.Environment (getArgs)
import GHC
    ( defaultErrorHandler,
      desugarModule,
      getInteractiveDynFlags,
      getModuleGraph,
      guessTarget,
      loadModule,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      typecheckModule,
      depanal,
      load,
      getNamesInScope,
      getSessionDynFlags,
      mgModSummaries,
      coreModule,
      ParsedMod(parsedSource),
      TypecheckedMod(typecheckedSource),
      LoadHowMuch(LoadAllTargets), compileToCoreSimplified, CoreModule (cm_module, cm_types, cm_binds), compileToCoreModule, TypecheckedModule (tm_renamed_source, tm_typechecked_source) )
import Data.List
import GHC.Driver.Pipeline (setDynFlags)
import GHC.Utils.Outputable
import GHC.Environment (getFullArgs)

main = do
    (filePath:_) <- getArgs
    getAST filePath

printFlags = do
    (flags, cm) <- runGhc (Just libdir) $ do
        f <- getSessionDynFlags
        return (f, (homeUnitId_ f))
    print $ cm == rtsUnitId

getAST filePath = defaultErrorHandler defaultFatalMessager defaultFlushOut  $ runGhc ghcFolder action
    where
        ghcFolder = Just libdir
        action = do
            flags <- getSessionDynFlags
            setSessionDynFlags (flags { ghcMode = CompManager })
            target <- guessTarget filePath Nothing
            setTargets $ Data.List.singleton target
            load LoadAllTargets
            modGraph <- depanal [] True
            -- Finding module Name in source file
            -- SRC: https://github.com/ghc/ghc/blob/994bda563604461ffb8454d6e298b0310520bcc8/compiler/GHC.hs#LL1287C25-L1287C37
            case find ((== filePath) . msHsFilePath) (mgModSummaries modGraph) of
                Just modsum -> do
                    -- Get AST of module
                    p <- parseModule modsum
                    -- Typecheck Module
                    t <- typecheckModule p
                    let (renamedSource) = tm_typechecked_source t
                    liftIO $ putStrLn $ showSDoc flags (ppr renamedSource)
                    -- Desugar Module
                    d <- desugarModule t
                    -- Compile Module
                    -- Might create .hi files
                    liftIO $ print $ ms_mod_name modsum
                    -- PANIC
                    -- l <- loadModule d
                    liftIO $ print $ ms_mod_name modsum
                    -- Might be names of resolved symbols 
                    -- n <- getNamesInScope
                    -- https://hackage.haskell.org/package/ghc-9.2.7/docs/GHC-Unit-Module-ModGuts.html#t:ModGuts
                    -- 'Digest' the compiled module
                    -- 
                    --let c = coreModule d
                    -- Get the module dependency graph
                    --g <- getModuleGraph
                    -- print $ showModule modsum
                    return (parsedSource d,"/n-----/n",  typecheckedSource d)
                Nothing -> panic "Module Name not found"
            --c <- compileToCoreModule filePath
            --liftIO $ print $ showSDoc flags $ ppr (cm_binds c)


