module TestMathStructures
(testMathStructures, mathStructureTests) where

import MathStructures
import LearnMath
import Test.HUnit
import Text.JSON
         
main = testMathStructures
testMathStructures = runTestTT $ mathStructureTests

mathStructureTests = TestList ([testRendering, baseCaseTests,arithmeticCases, testTopLevelExprsAdd, testRendering])

baseCaseTests = TestLabel "Base Cases" (TestList [testInt, testConst, testVar, testVarExpr])
arithmeticCases = TestLabel "Arithmetic Cases" (TestList [testAdd, testAddV, testAddDiff, testSub, 
                                                          testMult, testDiv, testPow, testLog, 
                                                          testNthRoot, testNthRootAlternate])
                                                          
testRendering = TestLabel "Rendering to JSON and HTML" (TestList [testJSON])
testJSON = TestLabel "Rendering to JSON" (TestList [testJSONReadEq, testJSONShowEq])

testJSONShowEq = TestCase $ assertEqual
               "should render an equation as JSON"
               "{\"equation\":{\"lhs\":{\"expression\":\"2 * x\"},\"rhs\":{\"expression\":\"4\"}}}" $
               (encode . showJSON $ (Equation (Binary Multiply (Nullary (Integ 2)) (Nullary (Variable "x"))) (Nullary (Integ 4))))
               
testJSONReadEq = TestCase $ assertEqual
                 "should read an equation from JSON"
                 "2 * x = 4" $
                 show (fromOkEq (decode "{\"equation\":{\"lhs\":{\"expression\":\"2 * x\"},\"rhs\":{\"expression\":\"4\"}}}" :: Result Equation))

testInt = TestCase $ assertEqual
          "should process a single integer as an expression"
          "1" $
          processExpression show "1"
          
testConst = TestCase $ assertEqual
          "should process a single floating point number as an expression"
          "1.0" $
          processExpression show "1.0"
          
testVar = TestCase $ assertEqual
          "should process a single variable as an expression"
          "x" $
          processExpression show "x"
          
testVarExpr = TestCase $ assertEqual
          "should process an adjacent number and a variable as an expression"
          "2 * x" $
          processExpression show "2x"
          
testAdd = TestCase $ assertEqual
          "should process an integer addition as an expression"
          "1 + 1" $
          processExpression show "1 + 1"
          
testAddV = TestCase $ assertEqual
          "should process a variable addition as an expression"
          "x + 1" $
          processExpression show "x + 1"
          
testAddDiff = TestCase $ assertEqual
          "should process addition of an integer and a double as an expression"
          "1 + 1.0" $
          processExpression show "1 + 1.0" 
          
testAddDiff2 = TestCase $ assertEqual
          "should process addition of a double and an integer as an expression"
          "1.0 + 1" $
          processExpression show "1.0 + 1"          
          
testSub = TestCase $ assertEqual
          "should process an integer subtraction as an expression"
          "1 - 1" $
          processExpression show "1 - 1"
          
testMult = TestCase $ assertEqual
          "should process an integer multiplication as an expression"
          "1 * 1" $
          processExpression show "1 * 1"
          
testDiv = TestCase $ assertEqual
          "should process an integer division as an expression"
          "1 / 1" $
          processExpression show "1 / 1"
          
testPow = TestCase $ assertEqual
          "should process an integer exponentiation as an expression"
          "1^1" $
          processExpression show "1^1"

testLog = TestCase $ assertEqual
          "should process an integer logarithm as an expression"
          "log<2>(4)" $
          processExpression show "log<2>(4)"
          
testNthRoot = TestCase $ assertEqual
              "should process an integer nthRoot as an expression"
              "<2>√(4)" $
              processExpression show "<2>√(4)"
          
testNthRootAlternate = TestCase $ assertEqual
                       "should process an integer nthRoot as an expression"
                       "<2>√(4)" $
                       processExpression show "root<2>(4)"
                       
testTopLevelExprsAdd = TestCase $ assertEqual
                       "should be able to break apart a tree into a list of all expressions using that op at the toplevel"
                       ["1","2", "3"] $
                       map show (topLevelExprs Add (parseExpression "1+2+3"))