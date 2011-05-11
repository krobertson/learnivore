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

main = runTestTT $ equationTests
-- 
equationTests = TestLabel "Equation Simplification Tests" (TestList [arithmeticTests, preAlgebraTests, algebraTests, bothSidesTests])
-- 
arithmeticTests = TestLabel "Arithmetic Tests" (TestList [tAdd, tSub, tMult, tDiv, tAbs, tNeg])

preAlgebraTests = TestLabel "Pre-Algebra Tests" (TestList [tLog, tPow])

algebraTests = TestLabel "Algebra Tests" (TestList [tVar1, tVar2, tVar3])

bothSidesTests = TestLabel "Equation Operator Tests" (TestList [tAddBothSides, tSubBothSides, tMultBothSides, tDivBothSides, tNegBothSides, tLogBothSides, tLogBothSides2, tPowBothSides, tPowBothSides2])


tAdd = TestCase $ assertEqual
          "should add two integers"
          "x = 2" $
          eqnSolution "x=1 + 1"
          
tSub = TestCase $ assertEqual
          "should subtract two integers"
          "x = 0" $
          eqnSolution  "x=1 - 1"
          
tMult = TestCase $ assertEqual
          "should multiply two integers"
          "x = 6" $
          eqnSolution  "x=2 * 3"
          
tDiv = TestCase $ assertEqual
          "should multiply two integers"
          "x = 2" $
          eqnSolution  "x=4 / 2"
          
tPow = TestCase $ assertEqual
          "should exponentiate two integers"
          "x = 8" $
          eqnSolution  "x=2 ^ 3"
          
tLog = TestCase $ assertEqual
          "should log two integers"
          "x = 2.0" $
          eqnSolution  "x=log<2>(4)"
          
tAbs = TestCase $ assertEqual
          "should multiply two integers"
          "x = 6" $
          eqnSolution  "x=|-6|"
          
tNeg = TestCase $ assertEqual
          "should multiply two integers"
          "x = -5" $
          eqnSolution  "x=-(3 + 2)"
          
tVar1 = TestCase $ assertEqual
          "should collapse two instances of the same variable"
          "x = 1" $
          eqnSolution  "x + x=2"
          
tVar2 = TestCase $ assertEqual
          "should collapse three instances of the same variable"
          "x = 1" $
          eqnSolution  "x + x + x=3"
          
tVar3 = TestCase $ assertEqual
          "should collapse three instances of the same variable"
          "x = 1" $
          eqnSolution  "2 * x + 2 * x=4"

tAddBothSides = TestCase $ assertEqual
          "should add 5 to both sides"
          "x = 1" $
          eqnSolution "x - 5=-4"
          
tSubBothSides = TestCase $ assertEqual
          "should subtract 1 from both sides"
          "x = 0" $
          eqnSolution "x + 1=1"
          
tNegBothSides = TestCase $ assertEqual
          "should negate both sides"
          "x = 1" $
          eqnSolution "-x=-1"
          
tMultBothSides = TestCase $ assertEqual
          "should multiply both sides by 5"
          "x = 1" $
          eqnSolution "x / 5=1 / 5"
          
tDivBothSides = TestCase $ assertEqual
          "should divide both sides by 5"
          "x = 1" $
          eqnSolution "x * 5=5"
          
tLogBothSides = TestCase $ assertEqual
          "should find x when it is the base of the exponentiation"
          "x = 2" $
          eqnSolution "x^2=4"
          
tLogBothSides2 = TestCase $ assertEqual
          "should find x when it is the power of an exponentiation"
          "x = 2.0" $
          eqnSolution "2^x=4"          
        
tPowBothSides = TestCase $ assertEqual
          "should find x when it is the argument of a logarithm"
          "x = 2" $
          eqnSolution "log<2>(x)=1"
          
tPowBothSides2 = TestCase $ assertEqual
          "should find x when it is the base of a logarithm"
          "x = 2" $
          eqnSolution "log<x>(4)=2"