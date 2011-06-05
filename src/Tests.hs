module Tests(testAll, allTests) where
import TestMathStructures
import TestExpressions
import TestEquations
import Test.HUnit
import Data.String.Utils


main = testAll
testAll = runTestTT $  allTests
allTests = TestLabel "Learnivore Tests" (TestList [mathStructureTests, expressionTests, equationTests])

-- testSolveEq sol lhs rhs = TestLabel ("Commutative test of " ++ lhs ++ " = " ++ rhs ++ " should equal: " ++ sol)
--                      (TestList [TestCase $ assertEqual
--                                 (lhs ++" = " ++ rhs ++ " should equal: " ++ sol)
--                                 sol $ eqnSolution (lhs ++ "=" ++ rhs),
--                                 TestCase $ assertEqual
--                                 (rhs ++" = " ++ lhs ++ " should equal: " ++ sol)
--                                 sol $ eqnSolution (rhs ++ "=" ++ lhs)])
--                                 
-- testSet setName 