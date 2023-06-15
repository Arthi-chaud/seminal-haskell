module TestCompiler.TypeChecker (testSuite) where

import Seminal.Compiler.Parser
import Test.HUnit ((@=?))
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Seminal.Compiler.TypeChecker (typecheckModule, TypeCheckStatus (Success, Error), ErrorType (TypeCheckError, ScopeError))
import Seminal.Compiler.Runner (runCompiler)

runTest :: [FilePath] -> IO TypeCheckStatus
runTest f = do
    Right [(_, m)] <- runCompiler f $ parseFiles f
    runCompiler f $ typecheckModule m

testSuite :: Test
testSuite = testGroup "Compiler's Typechecker" [
    buildTest $ do
        res <- runTest ["test/assets/valid/simple-main.hs"]
        return $ testCase "Typecheck Success" $ res @=? Success,
    buildTest $ do
        res <- runTest ["test/assets/invalid/type-error.hs"]
        return $ testCase "Typecheck Fail (Expected String, got Char)" $ res @=? Error (TypeCheckError ""),
    buildTest $ do
        res <- runTest ["test/assets/invalid/scope-error.hs"]
        return $ testCase "Typecheck Fail (Variable out of scope)" $ res @=? Error (ScopeError "")
    ]