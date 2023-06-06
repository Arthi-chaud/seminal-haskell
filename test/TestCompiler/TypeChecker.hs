module TestCompiler.TypeChecker (testSuite) where

import Seminal.Compiler.Parser
import Test.HUnit ((@=?))
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Seminal.Compiler.TypeChecker (typecheckModule, TypeCheckStatus (Success, Error), ErrorType (TypeCheckError, ScopeError))
import Seminal.Compiler.Runner

testSuite :: Test
testSuite = testGroup "Compiler's Typechecker" [
    buildTest $ do
        Right m <- parseFile "test/assets/valid/simple-main.hs"
        res <- runCompiler $ typecheckModule m
        return $ testCase "Typecheck Success" $ res @=? Success,
    buildTest $ do
        Right m <- parseFile "test/assets/invalid/type-error.hs"
        res <- runCompiler $ typecheckModule m
        return $ testCase "Typecheck Fail (Expected String, got Char)" $ res @=? Error (TypeCheckError ""),
    buildTest $ do
        Right m <- parseFile "test/assets/invalid/scope-error.hs"
        res <- runCompiler $ typecheckModule m
        return $ testCase "Typecheck Fail (Variable out of scope)" $ res @=? Error (ScopeError "")
    ]