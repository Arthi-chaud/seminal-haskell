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
    -- | Index of the suggestion
    -> Int
    -> IO Test
testSeminal files name expectedSrc expectedExec index = do
    res <- runSeminal (Options Eager False) (buildAssetPath <$> files)
    return $ testCase name $ case res of
        Result (_, [(_, _, changelist)]) -> let
            bestChange = changelist !! index in do
            show (pretty (src bestChange)) @?= expectedSrc
            show (pretty (head $ exec bestChange)) @?= expectedExec
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
        "[\"a\"]"  "'a'" 0,
    testSeminal
        ["expect-char-in-tuple"]
        "Got a string, expected a char, in a typed tuple" 
        "\"1\""  "'1'" 0,
    testSeminal
        ["expect-char"]
        "Got a string, expected a char" 
        "\"a\""  "'a'" 0,
    testSeminal
        ["expect-foldable"]
        "Got an int, expected a list (for overloaded function)" 
        "1"  "[1]" 0,
    testSeminal
        ["expect-item"]
        "Got a list, expected an item" 
        "['a']"  "'a'" 0,
    testSeminal
        ["expect-list-not-tuple"]
        "Got an tuple, expected a list" 
        "('a', 'b')" "['a', 'b']" 0,
    testSeminal
        ["expect-list"]
        "Got an int, expected a list" 
        "1"  "[1]" 0,
    testSeminal
        ["expect-string-in-list"]
        "List of String, with a Char in the middle" 
        "'A'"  "\"A\"" 0,
    testSeminal
        ["expect-string-not-int"]
        "Got an int, expected a string" 
        "1"  "show 1" 0,
    testSeminal
        ["expect-string"]
        "Got a Char, expected a String" 
        "'A'"  "\"A\"" 0,
    testSeminal
        ["expect-tuple"]
        "Got a list, expected a tuple" 
        "['a', 'b']"  "('a', 'b')" 0,
    testSeminal
        ["expect-unit"]
        "Got an Int, expected a '()'" 
        "1"  "()" 0
    ]

testLet :: [IO Test]
testLet = [
    testSeminal
        ["let/expect-char"]
        "Let: Got a String, expected a char" 
        "\"a\""  "'a'" 0
    ]

testWhere :: [IO Test]
testWhere = [
    testSeminal
        ["where/expect-char"]
        "Where: Got a String, expected a char" 
        "\"a\""  "'a'" 0
    ]

testIf :: [IO Test]
testIf = [
    testSeminal
        ["if/then-expect-int"]
        "If-Then: Got a List, expected an Int" 
        "[1]" "1" 0,
    testSeminal
        ["if/if-expect-bool"]
        "If: Got an Int, expected a Bool (Parm Swap)" 
        "const 1 True" "const True 1" 0,
    testSeminal
        ["if/else-expect-string"]
        "If-Else: Got a Char, expected a string" 
        "'.'" "\".\"" 0
    ]

testCases :: [IO Test]
testCases = [
    testSeminal
        ["case/match-expect-char"]
        "Case-Match: Got a List, expected a Char" 
        "\"L\"" "'L'" 0,
    testSeminal
        ["case/match-expect-wildcard"]
        "Case-Match: Got an Int, expected a wildcard" 
        "1" "_" 0,
    testSeminal
        ["case/root-expect-list"]
        "Case-Root: Got a Tuple, expected a List" 
        "(1, 2)" "[1, 2]" 0,
    testSeminal
        ["case/value-expect-int"]
        "Case-Value: Got a List, expected an Int" 
        "[2]" "2" 0
    ]

testModules :: [IO Test]
testModules = [
    testSeminal ["modules/A", "modules/B", "modules/Main"]
        "Modules: Got a Char, expected a String" 
        "a" "[a]" 0
    ]

testOperation :: [IO Test]
testOperation = [
    testSeminal
        ["operations/plus-expect-int"]
        "Operations (+): Got a List, expected an Int" 
        "[1]" "1" 0,
    testSeminal
        ["operations/neg-expect-int"]
        "Operations (-): Got a List, expected an Int" 
        "[1]" "1" 0
    ]

testApplication :: [IO Test]
testApplication = [
    testSeminal
        ["application/remove-first-param"]
        "Application: Remove First Param" 
        "head 1 [2]" "head [2]" 0,
    testSeminal
        ["application/remove-only-param"]
        "Application: Remove only Param" 
        "head 1" "head" 0,
    testSeminal
        ["application/remove-last-param"]
        "Application: Remove Last Param" 
        "fmap (+ 1) [1, 2, 3] 1" "fmap (+ 1) [1, 2, 3]" 0,
    testSeminal
        ["application/swap-first-and-last"]
        "Application: Swap First and Last Param" 
        "b 'a' 1 [3]" "b [3] 1 'a'" 2,
    testSeminal
        ["application/swap-first-two"]
        "Application: Swap First and Second Param" 
        "b 'a' 1 [3]" "b 1 'a' [3]" 2
    ]

testDo :: [IO Test]
testDo = [
    testSeminal
        ["do/final-expect-return"]
        "Do expression: Missing 'return' in final statement" 
        "()" "return ()" 0,
    testSeminal
        ["do/final-expect-unit"]
        "Do expression: Wrong type after 'return'" 
        "1" "()" 0
    ]

testSignatures :: [IO Test]
testSignatures = [
    testSeminal
        ["signatures/either-wrong-order"]
        "`Either` Signature: Swap type arguments" 
        "a :: Either Bool Int" "a :: Either Int Bool" 0,
    testSeminal
        ["signatures/either-wrong-right"]
        "`Either` Signature: Change second argument" 
        "a :: Either () Int" "a :: Either () String" 0,
    testSeminal
        ["signatures/expect-atomic-not-list"]
        "Signature: Replace `[Char]` with `Char`" 
        "a :: [Char]" "a :: Char" 0,
    testSeminal
        ["signatures/expect-atomic"]
        "Signature: Replace `Maybe Int` with `Int`" 
        "a :: Maybe Int" "a :: Int" 0,
    testSeminal
        ["signatures/expect-bool-list"]
        "Signature: Replace `[Int]` with `[Bool]`" 
        "a :: [Int]" "a :: [Bool]" 0,
    testSeminal
        ["signatures/expect-bool"]
        "Signature: Replace Int with Bool" 
        "a :: Int" "a :: Bool" 0,
    testSeminal
        ["signatures/expect-int"]
        "Signature: Replace Integer with Int" 
        "a :: Integer" "a :: Int" 0,
    testSeminal
        ["signatures/expect-return-bool"]
        "Signature: Change Return Type" 
        "a :: Int -> Int" "a :: Int -> Bool" 0,
    testSeminal
        ["signatures/expect-unit"]
        "Signature: Replace String with `()`" 
        "a :: String" "a :: ()" 0,
    testSeminal
        ["signatures/missing-io"]
        "Signature: Add Missing `IO`" 
        "a :: ()" "a :: IO ()" 1,
    testSeminal
        ["signatures/missing-leading-arg"]
        "Signature: Add Missing Parameter Type (Leading)" 
        "a :: Int" "a :: String -> Int" 0,
    testSeminal
        ["signatures/missing-middle-arg"]
        "Signature: Add Missing Parameter Type (Middle)" 
        "a :: Int -> Int" "a :: Int -> Int -> Int" 0,
    testSeminal
        ["signatures/missing-trailing-arg"]
        "Signature: Add Missing Parameter Type (Trailing)" 
        "a :: String" "a :: String -> Int" 0,
    testSeminal
        ["signatures/superfluous-leading"]
        "Signature: Replace `Int -> True` with `True`" 
        "a :: Int -> Bool" "a :: Bool" 0,
    testSeminal
        ["signatures/superfluous-middle"]
        "Signature: Remove Type in the middle" 
        "a :: [Char] -> Int -> Char" "a :: [Char] -> Char" 0,
    testSeminal
        ["signatures/superfluous-trailing"]
        "Signature: Replace `[Bool] -> ()` with `[Bool]`" 
        "a :: [Bool] -> ()" "a :: [Bool]" 0,
    testSeminal
        ["signatures/swap"]
        "Signature: Swap type parameters" 
        "a :: Int -> String" "a :: String -> Int" 0,
    testSeminal
        ["signatures/wrong-monad-child"]
        "Signature: Replace Int with ()" 
        "a :: Maybe Int" "a :: Maybe ()" 0,
    testSeminal
        ["signatures/wrong-monad-unit-child"]
        "Signature: Replace () with Int" 
        "a :: Maybe ()" "a :: Maybe Int" 0,
    testSeminal
        ["signatures/wrong-monad"]
        "Signature: Replace Maybe with IO" 
        "main :: Maybe ()" "main :: IO ()" 0
    ]