import MathStructures
import Test.HUnit
         
main = runTestTT $ TestList (baseCaseTests:arithmeticCases:[])

baseCaseTests = TestLabel "Base Cases" (TestList [testInt, testConst, testVar])
arithmeticCases = TestLabel "Arithmetic Cases" (TestList [testAdd, testAddV, testAddDiff, testSub, 
                                                          testMult, testDiv, testPow, testLog])

testInt = TestCase $ assertEqual
          "should process a single integer as an expression"
          "1" $
          processExpression id "1"
          
testConst = TestCase $ assertEqual
          "should process a single floating point number as an expression"
          "1.0" $
          processExpression id "1.0"
          
testVar = TestCase $ assertEqual
          "should process a single variable as an expression"
          "x" $
          processExpression id "x"
          
testAdd = TestCase $ assertEqual
          "should process an integer addition as an expression"
          "1 + 1" $
          processExpression id "1 + 1"
          
testAddV = TestCase $ assertEqual
          "should process a variable addition as an expression"
          "x + 1" $
          processExpression id "x + 1"
          
testAddDiff = TestCase $ assertEqual
          "should process addition of an integer and a double as an expression"
          "1 + 1.0" $
          processExpression id "1 + 1.0" 
          
testAddDiff2 = TestCase $ assertEqual
          "should process addition of a double and an integer as an expression"
          "1.0 + 1" $
          processExpression id "1.0 + 1"          
          
testSub = TestCase $ assertEqual
          "should process an integer subtraction as an expression"
          "1 - 1" $
          processExpression id "1 - 1"
          
testMult = TestCase $ assertEqual
          "should process an integer multiplication as an expression"
          "1 * 1" $
          processExpression id "1 * 1"
          
testDiv = TestCase $ assertEqual
          "should process an integer division as an expression"
          "1 / 1" $
          processExpression id "1 / 1"
          
testPow = TestCase $ assertEqual
          "should process an integer exponentiation as an expression"
          "1^1" $
          processExpression id "1^1"

testLog = TestCase $ assertEqual
          "should process an integer logarithm as an expression"
          "log<2>(4)" $
          processExpression id "log<2>(4)"