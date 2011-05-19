module Tests(testAll, allTests) where
import TestMathStructures
import TestExpressions
import TestEquations
import Test.HUnit


main = testAll
testAll = runTestTT $  allTests
allTests = TestLabel "Learnivore Tests" (TestList [mathStructureTests, expressionTests, equationTests])