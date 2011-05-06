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
arithmeticTests = TestLabel "Arithmetic Tests" (TestList [tAdd, tSub, tMult])--, tDiv, tLog, tPow, tAbs, tNeg])

preAlgebraTests = TestLabel "Algebra Tests" (TestList [])

-- type ApproximateDouble = AbsolutelyApproximateValue (Digits Three)
-- 
-- wrapAD :: Double -> ApproximateDouble Double
-- wrapAD = AbsolutelyApproximateValue
-- unwrapAD :: ApproximateDouble Double -> Double
-- unwrapAD = unwrapAbsolutelyApproximateValue

tAdd = TestCase $ assertEqual
          "should add two integers"
          "1 + 1\n=>\n2" $
          solveExpression "1 + 1"
          
tSub = TestCase $ assertEqual
          "should subtract two integers"
          "1 - 1\n=>\n0" $
          solveExpression "1 - 1"
          
tMult = TestCase $ assertEqual
          "should subtract two integers"
          "1 * 2\n=>\n2" $
          solveExpression "1 * 1"