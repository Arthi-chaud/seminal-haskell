module Main (main) where

import GHC.Paths
import GHC.Plugins
    ( DynFlags(ghcMode),
      GhcMode(CompManager),
      panic,
      defaultFatalMessager,
      defaultFlushOut,
      msHsFilePath, unLoc, showSDoc, Outputable (ppr), showSDocUnsafe )
import GHC
    ( defaultErrorHandler,
      guessTarget,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      depanal,
      getSessionDynFlags,
      mgModSummaries, ParsedModule (pm_parsed_source),
      HsModule (hsmodImports, hsmodExports, hsmodName, hsmodDecls), moduleNameString, HsDecl )
import Data.List ( find, singleton, intercalate )
import Text.Printf ( printf )
import Data.Functor
import GHC.Hs
import Control.Monad.IO.Class
import Data.Maybe (catMaybes, mapMaybe)

showGHC :: Outputable a => a -> String
showGHC = showSDocUnsafe . ppr

main :: IO ()
main = let filePath = "poc/assets/expressions.hs" in do
    m <- getModule filePath
    putStrLn $ printf "Module Name: %s" (getModuleName m)
    putStrLn $ printf "Module Imports: [%s]" (intercalate ", " (getModuleImports m))
    putStrLn $ printf "Module Declaration: [%s]" (intercalate ", " $ mapMaybe getDeclarationName  (getModuleDeclarations m))
    return ()

getModuleImports :: GHC.HsModule -> [String]
getModuleImports pm = show . moduleNameString . unLoc . ideclName <$> importDeclarations
    where
        importDeclarations = unLoc <$> hsmodImports pm

getDeclarationName :: HsDecl GhcPs -> Maybe String
-- Function declaration
-- Var declaration
getDeclarationName (ValD _ (FunBind _ id _ _)) = return $ showGHC (unLoc id)
getDeclarationName _ = Nothing

getModuleDeclarations :: HsModule -> [HsDecl GhcPs]
getModuleDeclarations m = unLoc <$> hsmodDecls m

getModuleName :: HsModule -> String
getModuleName x = maybe "*Unknown*" (show . unLoc) (hsmodName x)

getModule :: String -> IO GHC.HsModule
getModule filePath = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc ghcFolder action
    where
        ghcFolder = Just libdir
        action = do
            flags <- getSessionDynFlags
            setSessionDynFlags (flags { ghcMode = CompManager })
            target <- guessTarget filePath Nothing
            setTargets $ Data.List.singleton target
            -- _ <- load LoadAllTargets
            modGraph <- depanal [] True
            -- Finding module Name in source file
            -- SRC: https://github.com/ghc/ghc/blob/994bda563604461ffb8454d6e298b0310520bcc8/compiler/GHC.hs#LL1287C25-L1287C37
            case find ((== filePath) . msHsFilePath) (mgModSummaries modGraph) of
                Just modsum -> do
                    pm <- parseModule modsum <&> (unLoc . pm_parsed_source)
                    liftIO $ print $ showSDoc flags . ppr <$> getModuleDeclarations pm
                    return pm
                Nothing -> panic "Module name not found"