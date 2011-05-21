module TestExpressions
(testExpressions, expressionTests) where
  
-- import System.Random
-- import IO.Unsafe
-- import TypeLevel.NaturalNumber
-- import Data.Eq.Approximate
import MathStructures
import Expressions
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

main = testExpressions
testExpressions = runTestTT $ expressionTests
-- 
expressionTests = TestLabel "Expression Simplification Tests" (TestList [arithmeticTests, preAlgebraTests, algebraTests])
-- 
arithmeticTests = TestLabel "Arithmetic Tests" (TestList [tAdd, tSub, tMult, tDiv, tAbs, tNeg])

preAlgebraTests = TestLabel "Pre-Algebra Tests" (TestList [tLog, tPow, tRoot, tLogBase, tPowBase])

algebraTests = TestLabel "Algebra Tests" (TestList [tVar1, tVar2, tVar3])


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
          "should divide two integers"
          "2.0" $
          exprSolution  "4 / 2"
          
tPow = TestCase $ assertEqual
          "should exponentiate two integers"
          "8" $
          exprSolution  "2 ^ 3"
          
tLog = TestCase $ assertEqual
          "should log two integers"
          "2.0" $
          exprSolution  "log<2>(4)"
        
tLogBase = TestCase $ assertEqual
              "should handle log<y>(y^x)"
              "x" $
              exprSolution  "log<y>(y^x)"
              
tPowBase = TestCase $ assertEqual
              "should handle y^(log<y>(x))"
              "x" $
              exprSolution  "log<y>(y^x)"
          
tRoot = TestCase $ assertEqual
        "should take the nth root of two integers"
        "1.189207115002721" $
        exprSolution "root<2>(4)"
          
tAbs = TestCase $ assertEqual
          "should take the absolute value of a negative integer"
          "6" $
          exprSolution  "|-6|"
          
tNeg = TestCase $ assertEqual
          "should negate an expression"
          "-5" $
          exprSolution  "-(3 + 2)"
          
tVar1 = TestCase $ assertEqual
          "should collapse two instances of the same variable"
          "2 * x" $
          exprSolution  "x + x"
          
tVar2 = TestCase $ assertEqual
          "should collapse three instances of the same variable"
          "3 * x" $
          exprSolution  "x + x + x"
          
tVar3 = TestCase $ assertEqual
          "should collapse three instances of the same variable"
          "4 * x" $
          exprSolution  "2 * x + 2 * x"
          