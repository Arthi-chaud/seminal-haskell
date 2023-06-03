module TestSeminal (testSuite) where

import Test.HUnit ((@?=), assertFailure)
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Seminal (runSeminal, Status (..))
import Changes (Change(doc), ChangeDoc(..))
import GHC.Plugins (showSDocUnsafe)

buildAssetPath :: [Char] -> [Char]
buildAssetPath filename = "test/assets/invalid/" ++ filename ++ ".hs"

showSrc :: ChangeDoc -> String
showSrc = showSDocUnsafe . pprSrc
showExec :: ChangeDoc -> String
showExec = showSDocUnsafe . pprExec

getBestSuggestion :: [Change node] -> ChangeDoc
getBestSuggestion = doc . last

testSeminal ::
    -- | Name of the file to run seminal on
    FilePath
    -- | Name of the test
    -> String
    -- | expected src
    -> String
    -- | Expectd exec
    -> String
    -> IO Test
testSeminal file name src exec = do
    res <- runSeminal $ buildAssetPath file
    return $ testCase name $ case res of
        Changes m -> let bestChange = getBestSuggestion m in do
            showSrc bestChange @?= src
            showExec bestChange @?= exec
        _ -> assertFailure "Seminal Failed"

testSuite :: Test
testSuite = testGroup "Seminal" $ buildTest <$>[
    testSeminal
        "expect-char"
        "Got a string, expected a char" 
        "\"a\""  "'a'",
    testSeminal
        "expect-item"
        "Got a list, expected an item" 
        "['a']"  "'a'",
    testSeminal
        "expect-list"
        "Got an int, expected a list" 
        "1"  "[1]",
    testSeminal
        "expect-string"
        "Got a Char, expected a String" 
        "'A'"  "\"A\"",
    testSeminal
        "expect-tuple"
        "Got a list, expected a tuple" 
        "['a', 'b']"  "('a', 'b')"   
    ]