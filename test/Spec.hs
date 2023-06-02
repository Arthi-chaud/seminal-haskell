import Test.Framework (defaultMain)
import TestCompiler.Parser
import TestCompiler.TypeChecker
import TestSeminal

main :: IO ()
main = do
    defaultMain [ 
        TestCompiler.Parser.testSuite,
        TestCompiler.TypeChecker.testSuite,
        TestSeminal.testSuite
        ]