module TestEquations
(testEquations, equationTests) where
  
-- import System.Random
-- import IO.Unsafe
-- import TypeLevel.NaturalNumber
-- import Data.Eq.Approximate
import MathStructures
import Equations
-- import Generators
import Test.HUnit

-- uncomment the below code after figuring out how to get QuickCheck to work
-- generateExpression = do let sizes = [basic, simple, moderate, complex, hard]
--                         size <- choose (0, (length sizes) - 1)
--                         i <- choose (0, (length arithmetic) - 1)
--                         return ((map (\x -> (sizes !! size) arithmetic randomInteger) arithmetic) !! i)

-- type ApproximateDouble = AbsolutelyApproximateValue (Digits Three)
-- 
-- wrapAD :: Double -> ApproximateDouble Double
-- wrapAD = AbsolutelyApproximateValue
-- unwrapAD :: ApproximateDouble Double -> Double
-- unwrapAD = unwrapAbsolutelyApproximateValue

main = testEquations
testEquations = runTestTT $ equationTests
-- 
equationTests = TestLabel "Equation Simplification Tests" (TestList [arithmeticTests, preAlgebraTests, algebraTests, bothSidesTests, tEquivalentEquations, tSolveThroughEqs, tSolveThroughEqs2])--, guessingTests])
-- 
arithmeticTests = TestLabel "Arithmetic Tests" (TestList [tAdd, tSub, tMult, tDiv, tAbs, tNeg])

preAlgebraTests = TestLabel "Pre-Algebra Tests" (TestList [tLog, tPow])

algebraTests = TestLabel "Algebra Tests" (TestList [tVar1, tVar2, tVar3])

bothSidesTests = TestLabel "Equation Operator Tests" (TestList [tAddBothSides, tSubBothSides, tMultBothSides, tDivBothSides, tLogBothSides, tLogBothSidesEdge, tPowBothSides, tPowBothSidesEdge, tNthRootBothSides, tFindLogarithmBase, tUnrootBothSides, tUnrootBothSides2])

guessingTests = TestLabel "Equations requiring guessing" (TestList [tHardEq1, tHardEq2, tHardEq3])

testSolveEq sol lhs rhs = TestLabel ("Commutative test of " ++ lhs ++ " = " ++ rhs ++ " should equal: " ++ sol)
                     (TestList [TestCase $ assertEqual
                                (lhs ++" = " ++ rhs ++ " should equal: " ++ sol)
                                sol $ eqnSolution (lhs ++ "=" ++ rhs),
                                TestCase $ assertEqual
                                (rhs ++" = " ++ lhs ++ " should equal: " ++ sol)
                                sol $ eqnSolution (rhs ++ "=" ++ lhs)])

tAdd = testSolveEq "x = 2" "x" "1 + 1" 
          
tSub = testSolveEq "x = 0" "x" "1 - 1"
          
tMult = testSolveEq "x = 6" "x" "2 * 3"  
          
tDiv = testSolveEq "x = 2.0" "x" "4 / 2"
          
tPow = testSolveEq "x = 8" "x" "2 ^ 3"
          
tLog = testSolveEq "x = 2.0" "x" "log<2>(4)"
          
tAbs = testSolveEq "x = 6" "x" "|-6|"
          
tNeg = testSolveEq "x = -5" "x" "-(3 + 2)"
          
tVar1 = testSolveEq "x = 1.0" "x + x" "2"
          
tVar2 = testSolveEq "x = 1.0" "x + x + x" "3"
          
tVar3 = testSolveEq "x = 1.0" "2 * x + 2 * x" "4"

tAddBothSides = testSolveEq "x = 1" "x - 5" "-4"
          
tSubBothSides = testSolveEq "x = 0" "x + 1" "1"
          
tNegBothSides = testSolveEq "x = -1" "-x" "1"
          
tMultBothSides = testSolveEq "x = 1.0" "x / 5" "1 / 5"
          
tDivBothSides = testSolveEq "x = 1.0" "x * 5" "5"
          
tNthRootBothSides = testSolveEq "x = 2.0" "x^3" "8"
          
tLogBothSides = testSolveEq "x = 2.0" "2^x" "4"

tLogBothSidesEdge = testSolveEq "x = 1.0" "2^x" "2"          
        
tPowBothSidesEdge = testSolveEq "x = 2" "log<2>(x)" "1"

tPowBothSides = testSolveEq "x = 16" "log<2>(x)" "4"
          
tUnrootBothSides = testSolveEq "x = 8" "root<3>(x)" "2"
          
tUnrootBothSides2 = testSolveEq "x = 3.0" "root<x>(8)" "2"
          
tFindLogarithmBase = testSolveEq "x = 2.0" "log<x>(4)" "2"

tHardEq1 = testSolveEq "x = 2" "(2^x+2) / x" "3"

tHardEq2 = testSolveEq "x = 2" "2^x + x" "6"

tHardEq3 = testSolveEq "x = 2" "2^x + x" "4+2"

tEquivalentEquations = TestCase $ assertEqual
          "should recognize two equivalent equations: 2^x = 4 => 10 * 2^x = 40"
          "2^x = 4.0" $
          equivalentEquations "2^x=4" "10 * 2^x=40"
          
tSolveThroughEqs = TestCase $ assertEqual
          "should find a solution through a given path"
          "x = 2\n=> Final Equation\nx = 2" $
          solveThroughEquations ["x=log<2>(4)", "x=2"] "2^x=4"
          
tSolveThroughEqs2 = TestCase $ assertEqual
          "should find a solution through a given path"
          "x = 2\n=> Final Equation\nx = 2" $
          solveThroughEquations ["2+x+1+1=6", "4+x=6", "x=6-4", "x=2"] "x+1+1+1+1=6"