module Main (main) where

import GHC.Paths ( libdir )
import GHC.Plugins
    ( DynFlags(ghcMode),
      GhcMode(CompManager),
      panic,
      defaultFatalMessager,
      defaultFlushOut,
      msHsFilePath, unLoc, Outputable (ppr), showSDocUnsafe )
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
      HsModule (hsmodImports, hsmodExports, hsmodName, hsmodDecls), moduleNameString, GenLocated (L) )
import Data.List ( find, singleton, intercalate )
import Text.Printf ( printf )
import Data.Functor
import GHC.Hs
import Debug.Trace

data DeclarationType = TypeSignature | FuncBind | ConstInt | ConstChar | ConstString | ConstArray | ConstTupple | ConstObj | Data | Type | Unknown deriving Show

data Declaration = Declaration {
    name :: String,
    declType :: DeclarationType
} deriving Show

showGHC :: Outputable a => a -> String
showGHC = showSDocUnsafe . ppr

main :: IO ()
main = let filePath = "poc/assets/expressions.hs" in do
    m <- getModule filePath
    putStrLn $ printf "Module Name: %s" (getModuleName m)
    putStrLn $ printf "Module Imports: [%s]" (intercalate ", " (getModuleImports m))
    putStrLn $ printf "Declaration: [\n\t%s\n]" (intercalate "\n\t" $ show . parseDeclaration <$> (getModuleDeclarations m))
    return ()

getModuleImports :: GHC.HsModule -> [String]
getModuleImports pm = show . moduleNameString . unLoc . ideclName <$> importDeclarations
    where
        importDeclarations = unLoc <$> hsmodImports pm


parseDeclaration :: HsDecl GhcPs -> Maybe Declaration
parseDeclaration (TyClD _ (SynDecl _ n _ _ _)) = return $ Declaration { name = showGHC $ unLoc n, declType = Type }
parseDeclaration (TyClD _ (DataDecl _ n _ _ _)) = return $ Declaration { name = showGHC $ unLoc n, declType = Data }
parseDeclaration (ValD _ (FunBind _ id (MG _ match _) _ )) = return $ Declaration { name = showGHC (unLoc id), declType = parseExprValue expr }
    where
        expr = unLoc e
        (GRHS _ _ e) = unLoc $ head $ grhssGRHSs $ m_grhss $ unLoc $ head (unLoc match)
parseDeclaration (SigD _ (TypeSig _ [id] _)) = return $ Declaration { name = showGHC (unLoc id), declType = TypeSignature }
parseDeclaration (ValD _ (PatBind _ (L _ (WildPat _)) _ _ )) = return $ Declaration { name = "_", declType = Data } 
parseDeclaration _ = Nothing


parseExprValue :: HsExpr GhcPs -> DeclarationType
parseExprValue (HsLit _ (HsInt _ _)) = ConstInt
parseExprValue (HsLit _ (HsIntPrim _ _)) = ConstInt
parseExprValue (HsLit _ (HsInt64Prim _ _)) = ConstInt
parseExprValue (HsLit _ (HsInteger _ _ _)) = ConstInt
parseExprValue (HsLit _ (HsRat _ _ _)) = ConstInt
parseExprValue (HsLit _ (HsChar _ _)) = ConstChar
parseExprValue (HsLit _ (HsCharPrim _ _)) = ConstChar
parseExprValue (HsLit _ (HsString _ _)) = ConstString
parseExprValue (HsApp _ _ _) = FuncBind
parseExprValue (ExplicitTuple _ _ _) = ConstTupple
parseExprValue (ExplicitList {}) = ConstArray
parseExprValue (RecordCon {}) = ConstObj
parseExprValue (HsOverLit _ e) = case ol_val e of
    HsIntegral _ -> ConstInt
    HsFractional _ -> ConstInt
    HsIsString _ _ -> ConstString
parseExprValue _ = Unknown



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
                Just modsum -> parseModule modsum <&> (unLoc . pm_parsed_source)
                Nothing -> panic "Module name not found"