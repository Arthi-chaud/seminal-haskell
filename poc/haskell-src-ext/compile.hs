{-# LANGUAGE ScopedTypeVariables #-}
import GHC.Paths ( libdir )
import System.Environment (getArgs)
import GHC
    ( guessTarget,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      typecheckModule,
      getSessionDynFlags,
      mgModSummaries, depanalE, ParsedModule, Ghc, ms_mod_name, moduleNameString, Target (..), TargetId (TargetModule), depanal, addTarget, getTargets )
import Data.List ( find, dropWhileEnd )
import Data.Monoid ()
import Control.Monad.Catch as MC
import GHC.Plugins (msHsFilePath, panic, mkModuleName, liftIO, showSDoc, Outputable (ppr))
import GHC.Types.SourceError
import Text.Printf (printf)
import Data.Char
import GHC.SysTools (isContainedIn)
import Language.Haskell.Exts (Module (Module), parseModule, ParseResult (..), SrcSpanInfo, prettyPrint, ModuleHead (ModuleHead), ModuleName (ModuleName))
import Data.Time (UTCTime(..), Day (ModifiedJulianDay))
import GHC.Data.StringBuffer
import GHC.Parser.Lexer (getErrorMessages)
data GHCStatus = Success | Error ErrorType String
data ErrorType = SyntaxError | TypeError | ScopeError deriving Show
-- Compile AST from HSE, using GHC-lib
-- Problem: Cannot Load Target from a String

main :: IO ()
main = do
    (filePath:_) <- getArgs
    ast <- parseFile filePath
    case ast of
        Module s h _ _ _ -> do
            let name = case h of
                    (Just (ModuleHead _ mname _ _)) -> let (ModuleName _ n) = mname in n
                    Nothing -> "Main"
            execGHC (prettyPrint ast) name >>= printGHCStatusMessage
        _ -> putStrLn "Can not decompose parsed module"
    -- Get AST from HSE
    -- Run Compilation

parseFile :: FilePath -> IO (Module SrcSpanInfo)
parseFile filePath = do
    content <- readFile filePath
    case Language.Haskell.Exts.parseModule content of
        ParseFailed _  _-> panic "HSC Fail: Parsing error"
        ParseOk m -> return m


-- From ghc-lib/catchError.hs
printGHCStatusMessage :: GHCStatus -> IO ()
printGHCStatusMessage status = putStrLn $ case status of
    Success -> "Compilation Successful"
    Error err msg -> dropWhileEnd isSpace $ printf "%s: %s" (show err) (unlines $ take 2 $ lines msg)

buildTarget :: String -> String -> Target
buildTarget moduleAsString moduleName = Target {
    targetId = TargetModule $ mkModuleName moduleName,
    targetAllowObjCode = True,
    targetContents = return (stringToStringBuffer moduleAsString, UTCTime { utctDay = (ModifiedJulianDay 0), utctDayTime = 0 })
}

execGHC :: String -> String -> IO GHCStatus
execGHC moduleAsString moduleName = runGhc ghcFolder action
    where
        ghcFolder = Just libdir
        action = do
            flags <- getSessionDynFlags
            setSessionDynFlags flags
            addTarget (buildTarget moduleAsString moduleName)
            targets <- getTargets
            liftIO $ print $ (showSDoc flags . ppr) <$> targets
            setTargets targets
            modGraph <- depanal [] True
            liftIO $ print  $ (moduleNameString . ms_mod_name) <$> mgModSummaries modGraph
            -- liftIO $ print $ showSDoc flags (getErrorMessages errs)
            case find ((== moduleName) . moduleNameString . ms_mod_name) (mgModSummaries modGraph) of
                Just modsum -> do
                    result <- try (GHC.parseModule modsum) :: Ghc (Either SourceError ParsedModule)
                    case result of
                        Left err -> return $ Error SyntaxError (show err)
                        Right p -> do
                            maybeT <- handleSourceError (return . Left . show) $ typecheckModule p >>= (return . Right)
                            return $ case maybeT of
                                Left errMsg -> Error (if "Variable not in scope" `isContainedIn` errMsg
                                    then ScopeError
                                    else TypeError) errMsg
                                Right _ -> Success
                Nothing -> panic "Module Name not found"