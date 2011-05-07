-- import System.Random
-- import IO.Unsafe
-- import TypeLevel.NaturalNumber
-- import Data.Eq.Approximate
import MathStructures
import Expressions
-- import Generators
import Test.HUnit

main = runTestTT $ expressionTests

-- uncomment the below code after figuring out how to get QuickCheck to work
-- generateExpression = do let sizes = [basic, simple, moderate, complex, hard]
--                         size <- choose (0, (length sizes) - 1)
--                         i <- choose (0, (length arithmetic) - 1)
--                         return ((map (\x -> (sizes !! size) arithmetic randomInteger) arithmetic) !! i)


-- uncomment the below code after implementing the tests in the list
-- main = runTestTT $ expressionTests
-- 
expressionTests = TestLabel "Expression Simplification Tests" (TestList [arithmeticTests, preAlgebraTests])
-- 
arithmeticTests = TestLabel "Arithmetic Tests" (TestList [tAdd, tSub, tMult, tDiv, tLog, tPow, tAbs, tNeg])

preAlgebraTests = TestLabel "Algebra Tests" (TestList [])

-- type ApproximateDouble = AbsolutelyApproximateValue (Digits Three)
-- 
-- wrapAD :: Double -> ApproximateDouble Double
-- wrapAD = AbsolutelyApproximateValue
-- unwrapAD :: ApproximateDouble Double -> Double
-- unwrapAD = unwrapAbsolutelyApproximateValue

tAdd = TestCase $ assertEqual
          "should add two integers"
          "2" $
          exprSolution "1 + 1"
          
tSub = TestCase $ assertEqual
          "should subtract two integers"
          "0" $
          exprSolution  "1 - 1"
          
tMult = TestCase $ assertEqual
          "should multiply two integers"
          "6" $
          exprSolution  "2 * 3"
          
tDiv = TestCase $ assertEqual
          "should multiply two integers"
          "2" $
          exprSolution  "4 / 2"
          
tPow = TestCase $ assertEqual
          "should exponentiate two integers"
          "8" $
          exprSolution  "2 ^ 3"
          
tLog = TestCase $ assertEqual
          "should log two integers"
          "2" $
          exprSolution  "log<2>(4)"
          
tAbs = TestCase $ assertEqual
          "should multiply two integers"
          "6" $
          exprSolution  "|-6|"
          
tNeg = TestCase $ assertEqual
          "should multiply two integers"
          "-5" $
          exprSolution  "-(3 + 2)"