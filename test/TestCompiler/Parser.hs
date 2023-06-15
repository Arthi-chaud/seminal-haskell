module TestCompiler.Parser (testSuite) where

import Seminal.Compiler.Parser
import Seminal.Compiler.Runner
import Test.HUnit ((@=?), assertFailure)
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import GHC (moduleNameString, moduleName, ms_mod, pm_mod_summary)
import System.Directory (getPermissions, setOwnerReadable, setPermissions)

runParser :: [FilePath] -> IO ParsingResult
runParser files = runCompiler files (parseFiles files)

testSuite :: Test
testSuite = testGroup "Compiler's Parser" [
    buildTest $ do
        res <- runParser ["test/assets/valid/simple-main.hs"]
        return $ testCase "Valid Parsing (Main - Implicit Module Name)" $ case res of
            Left _ -> assertFailure "Parsing Failed"
            Right m -> (moduleNameString. moduleName . ms_mod . pm_mod_summary . snd . head $ m) @=? "Main",
    buildTest $ do
        res <- runParser ["test/assets/valid/explicit-main-module.hs"]
        return $ testCase "Valid Parsing (Main - Explicit Module Name)" $ case res of
            Left _ -> assertFailure "Parsing Failed"
            Right m -> (moduleNameString. moduleName . ms_mod . pm_mod_summary . snd . head $ m) @=? "Main",
    buildTest $ do
        res <- runParser ["test/assets/invalid/syntax-error.hs"]
        return $ testCase "Invalid Parsing (Syntax Error)" $ case res of
            Left [(_, e)] -> e @=? SyntaxError
            _ -> assertFailure "Parsing should have failed",
    buildTest $ do
        res <- runParser ["idonotexist"]
        return $ testCase "Invalid Parsing (File Not Found)" $ case res of
            Left [(_, e)] -> e @=? FileNotFound
            _ -> assertFailure "Parsing should have failed",
    buildTest $ do
        let filePath = "test/assets/valid/explicit-main-module.hs"
        _ <- getPermissions filePath >>= setPermissions filePath . setOwnerReadable False
        res <- runParser [filePath]
        _ <- getPermissions filePath >>= setPermissions filePath . setOwnerReadable True
        return $ testCase "Invalid Parsing (Pemrission denied)" $ case res of
            Left [(_, e)] -> e @=? PermissionDenied
            _ -> assertFailure "Parsing should have failed"
    ]