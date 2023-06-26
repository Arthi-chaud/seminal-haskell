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
    -- | Name of the files to run seminal on
    [FilePath]
    -- | Name of the test
    -> String
    -- | expected src
    -> String
    -- | Expectd exec
    -> String
    -> IO Test
testSeminal files name expectedSrc expectedExec = do
    res <- runSeminal (Options Eager False) (buildAssetPath <$> files)
    return $ testCase name $ case res of
        Result (_, [(_, _, bestChange:_)]) -> do
            (show $ pretty $ src bestChange) @?= expectedSrc
            (show $ pretty $ head $ exec bestChange) @?= expectedExec
        _ -> assertFailure "Seminal Failed"

testSuite :: Test
testSuite = testGroup "Seminal" $ buildTest <$> testGroups
    where
        testGroups = concat [
            testRoot,
            testLet,
            testDo,
            testApplication,
            testWhere,
            testIf,
            testModules,
            testOperation,
            testSignatures,
            testCases
            ]

testRoot :: [IO Test]
testRoot = [
    testSeminal
        ["expect-char-from-string-list"]
        "Got a singleton of string, expected a char" 
        "[\"a\"]"  "'a'",
    testSeminal
        ["expect-char-in-tuple"]
        "Got a string, expected a char, in a typed tuple" 
        "\"1\""  "'1'",
    testSeminal
        ["expect-char"]
        "Got a string, expected a char" 
        "\"a\""  "'a'",
    testSeminal
        ["expect-foldable"]
        "Got an int, expected a list (for overloaded function)" 
        "1"  "[1]",
    testSeminal
        ["expect-item"]
        "Got a list, expected an item" 
        "['a']"  "'a'",
    testSeminal
        ["expect-list-not-tuple"]
        "Got an tuple, expected a list" 
        "('a', 'b')" "['a', 'b']",
    testSeminal
        ["expect-list"]
        "Got an int, expected a list" 
        "1"  "[1]",
    testSeminal
        ["expect-string-in-list"]
        "List of String, with a Char in the middle" 
        "'A'"  "\"A\"",
    testSeminal
        ["expect-string-not-int"]
        "Got an int, expected a string" 
        "1"  "show 1",
    testSeminal
        ["expect-string"]
        "Got a Char, expected a String" 
        "'A'"  "\"A\"",
    testSeminal
        ["expect-tuple"]
        "Got a list, expected a tuple" 
        "['a', 'b']"  "('a', 'b')",
    testSeminal
        ["expect-unit"]
        "Got an Int, expected a '()'" 
        "1"  "()"
    ]

testLet :: [IO Test]
testLet = [
    testSeminal
        ["let/expect-char"]
        "Let: Got a String, expected a char" 
        "\"a\""  "'a'"
    ]

testWhere :: [IO Test]
testWhere = [
    testSeminal
        ["where/expect-char"]
        "Where: Got a String, expected a char" 
        "\"a\""  "'a'"
    ]

testIf :: [IO Test]
testIf = [
    testSeminal
        ["if/then-expect-int"]
        "If-Then: Got a List, expected an Int" 
        "[1]" "1",
    testSeminal
        ["if/if-expect-bool"]
        "If: Got an Int, expected a Bool (Parm Swap)" 
        "const 1 True" "const True 1",
    testSeminal
        ["if/else-expect-string"]
        "If-Else: Got a Char, expected a string" 
        "'.'" "\".\""
    ]

testCases :: [IO Test]
testCases = [
    testSeminal
        ["case/match-expect-char"]
        "Case-Match: Got a List, expected a Char" 
        "\"L\"" "'L'",
    testSeminal
        ["case/match-expect-wildcard"]
        "Case-Match: Got an Int, expected a wildcard" 
        "1" "_",
    testSeminal
        ["case/root-expect-list"]
        "Case-Root: Got a Tuple, expected a List" 
        "(1, 2)" "[1, 2]",
    testSeminal
        ["case/value-expect-int"]
        "Case-Value: Got a List, expected an Int" 
        "[2]" "2"
    ]

testModules :: [IO Test]
testModules = [
    testSeminal ["modules/A", "modules/B", "modules/Main"]
        "Modules: Got a Char, expected a String" 
        "a" "[a]"
    ]

testOperation :: [IO Test]
testOperation = [
    testSeminal
        ["operations/plus-expect-int"]
        "Operations (+): Got a List, expected an Int" 
        "[1]" "1",
    testSeminal
        ["operations/neg-expect-int"]
        "Operations (-): Got a List, expected an Int" 
        "[1]" "1"
    ]

testApplication :: [IO Test]
testApplication = [
    testSeminal
        ["application/remove-first-param"]
        "Application: Remove First Param" 
        "head 1 [2]" "head [2]",
    testSeminal
        ["application/remove-only-param"]
        "Application: Remove only Param" 
        "head 1" "head",
    testSeminal
        ["application/remove-last-param"]
        "Application: Remove Last Param" 
        "fmap (+ 1) [1, 2, 3] 1" "fmap (+ 1) [1, 2, 3]",
    testSeminal
        ["application/swap-first-and-last"]
        "Application: Swap First and Last Param" 
        "b 'a' 1 [3]" "b [3] 1 'a'",
    testSeminal
        ["application/swap-first-two"]
        "Application: Swap First and Second Param" 
        "b 'a' 1 [3]" "b 1 'a' [3]"
    ]

testDo :: [IO Test]
testDo = [
    testSeminal
        ["do/final-expect-return"]
        "Do expression: Missing 'return' in final statement" 
        "()" "return ()",
    testSeminal
        ["do/final-expect-unit"]
        "Do expression: Wrong type after 'return'" 
        "1" "()"
    ]

testSignatures :: [IO Test]
testSignatures = [
    testSeminal
        ["signatures/expect-atomic"]
        "Signature: Replace `Maybe Int` with `Int`" 
        "a :: Maybe Int" "a :: Int",
    testSeminal
        ["signatures/expect-bool"]
        "Signature: Replace Int with Bool" 
        "a :: Int" "a :: Bool",
    testSeminal
        ["signatures/expect-int"]
        "Signature: Replace Integer with Int" 
        "a :: Integer" "a :: Int",
    testSeminal
        ["signatures/expect-unit"]
        "Signature: Replace String with `()`" 
        "a :: String" "a :: ()",
    testSeminal
        ["signatures/wrong-monad-child"]
        "Signature: Replace Int with ()" 
        "a :: Maybe Int" "a :: Maybe ()",
    testSeminal
        ["signatures/wrong-monad"]
        "Signature: Replace Maybe with IO" 
        "main :: Maybe ()" "main :: IO ()"
    ]