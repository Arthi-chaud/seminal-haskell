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
      mgModSummaries, depanalE, ParsedModule, Ghc )
import Data.List ( find, dropWhileEnd )
import Data.Monoid ()
import Control.Monad.Catch as MC
import GHC.Plugins (msHsFilePath, panic)
import GHC.Types.SourceError
import Text.Printf (printf)
import Data.Char
import GHC.SysTools (isContainedIn)
-- Try to catch Error from

data GHCStatus = Success | Error ErrorType String

data ErrorType = SyntaxError | TypeError | ScopeError deriving Show
main :: IO ()
main = do
    (filePath:_) <- getArgs
    result <- execGHC filePath
    printGHCStatusMessage result

printGHCStatusMessage :: GHCStatus -> IO ()
printGHCStatusMessage status = putStrLn $ case status of
    Success -> "Compilation Successful"
    Error err msg -> dropWhileEnd isSpace $ printf "%s: %s" (show err) (unlines $ take 2 $ lines msg)

execGHC :: String -> IO GHCStatus
execGHC filePath = runGhc ghcFolder action
    where
        ghcFolder = Just libdir
        action = do
            flags <- getSessionDynFlags
            setSessionDynFlags flags
            target <- guessTarget filePath Nothing
            setTargets [target]
            (errs, modGraph) <- depanalE [] True
            case find ((== filePath) . msHsFilePath) (mgModSummaries modGraph) of
                Just modsum -> do
                    result <- try (parseModule modsum) :: Ghc (Either SourceError ParsedModule)
                    case result of
                        Left err -> return $ Error SyntaxError (show err)
                        Right p -> do
                            maybeT <- handleSourceError (return . Left . show) $ typecheckModule p >>= (return . Right)
                            return $ case maybeT of
                                Left errMsg -> Error (if any (\x -> isContainedIn x errMsg) ["Variable not in scope", "Not in scope: type constructor or class"]
                                    then ScopeError
                                    else TypeError) errMsg
                                Right _ -> Success
                Nothing -> panic "Module Name not found"