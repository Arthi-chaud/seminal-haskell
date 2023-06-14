module TestSeminal (testSuite) where

import Test.HUnit ((@?=), assertFailure)
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Seminal (runSeminal, Status (..))
import Seminal.Change (Change(..), ChangeNode(pretty))
import Seminal.Options

buildAssetPath :: [Char] -> [Char]
buildAssetPath filename = "test/assets/invalid/" ++ filename ++ ".hs"

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
testSeminal file name expectedSrc expectedExec = do
    res <- runSeminal (Options Eager) [buildAssetPath file]
    return $ testCase name $ case res of
        Changes [(_, _, (bestChange:_))] -> do
            (show $ pretty $ src bestChange) @?= expectedSrc
            (show $ pretty $ exec bestChange) @?= expectedExec
        _ -> assertFailure $ "Seminal Failed"

testSuite :: Test
testSuite = testGroup "Seminal" $ buildTest <$> [
    testSeminal
        "expect-char-from-string-list"
        "Got a singleton of string, expected a char" 
        "[\"a\"]"  "'a'",
    testSeminal
        "expect-char-in-tuple"
        "Got a string, expected a char, in a typed tuple" 
        "\"1\""  "'1'",
    testSeminal
        "expect-char"
        "Got a string, expected a char" 
        "\"a\""  "'a'",
    testSeminal
        "expect-foldable"
        "Got an int, expected a list (for overloaded function)" 
        "1"  "[1]",
    testSeminal
        "expect-item"
        "Got a list, expected an item" 
        "['a']"  "'a'",
    testSeminal
        "expect-list-not-tuple"
        "Got an tuple, expected a list" 
        "('a', 'b')" "['a', 'b']",
    testSeminal
        "expect-list"
        "Got an int, expected a list" 
        "1"  "[1]",
    testSeminal
        "expect-string-in-list"
        "List of String, with a Char in the middle" 
        "'A'"  "\"A\"",
    testSeminal
        "expect-string-not-int"
        "Got an int, expected a string" 
        "1"  "show 1",
    testSeminal
        "expect-string"
        "Got a Char, expected a String" 
        "'A'"  "\"A\"",
    testSeminal
        "expect-tuple"
        "Got a list, expected a tuple" 
        "['a', 'b']"  "('a', 'b')",
    testSeminal
        "let/expect-char"
        "Let: Got a String, expected a char" 
        "\"a\""  "'a'",
    testSeminal
        "where/expect-char"
        "Where: Got a String, expected a char" 
        "\"a\""  "'a'",
    testSeminal
        "if/then-expect-int"
        "If-Then: Got a List, expected an Int" 
        "[1]" "1",
    testSeminal
        "if/if-expect-bool"
        "If: Got an Int, expected a Bool" 
        "1" "True",
    testSeminal
        "if/else-expect-string"
        "If-Else: Got a Char, expected a string" 
        "'.'" "\".\"",
    testSeminal
        "case/match-expect-char"
        "Case-Match: Got a List, expected a Char" 
        "\"L\"" "'L'",
    testSeminal
        "case/root-expect-list"
        "Case-Root: Got a Tuple, expected a List" 
        "(1, 2)" "[1, 2]",
    testSeminal
        "case/value-expect-int"
        "Case-Value: Got a List, expected an Int" 
        "[2]" "2"
    ]