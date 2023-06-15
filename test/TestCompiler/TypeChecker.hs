module TestCompiler.TypeChecker (testSuite) where

import Test.HUnit ((@=?), assertFailure)
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Seminal.Compiler.TypeChecker (typecheckModule, TypeCheckStatus (Success, Error), ErrorType (TypeCheckError, ScopeError))
import Seminal.Compiler.Runner (runCompiler)

runTest :: [FilePath] -> IO TypeCheckStatus
runTest f = do
    res <- runCompiler f (\c -> case c of
        [(_, m)] -> typecheckModule m
        _ -> return $ Error (TypeCheckError "")
        )
    case res of
        Right m -> return m
        Left _ -> assertFailure "Test failed"

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