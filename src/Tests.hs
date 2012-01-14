module Tests(testAll, allTests) where
import TestMathStructures
import TestExpressions
import TestEquations
import TestAlgebraicStructures
import Test.HUnit
import Data.List
import Data.String.Utils
import Data.Object
import Data.Object.Yaml
import Data.String.Utils
import System.IO.Unsafe

main = testAll
testAll = runTestTT $  allTests
allTests = TestLabel "Learnivore Tests" (TestList [mathStructureTests, expressionTests, equationTests, mathTests, algebraicStructureTests])
mathTests = importTests ["prealgebra.yml", "trigonometry.yml"]
                       

importTests files = TestLabel "Mathematics Tests" (TestList (map decodeTestSet files))
decodeTestSet file = unsafePerformIO (do {maybeObject <- decodeFile file
										 											;case maybeObject of
										 	 											Nothing -> return (TestCase $ assertEqual ("tests for " ++ file ++ " failed to load due to a YAML parse error.") True $ False)
										 	 											Just object -> return (decodeTests object)})
					
decodeTests (Mapping [(setName, Mapping testLists)]) = TestLabel setName (TestList $ map decodeTestList testLists)						

decodeTestList (listName, Sequence tests) =  TestLabel listName (TestList $ (map decodeTest tests))

decodeTest :: Object [Char] [Char] -> Test
decodeTest (Mapping [("eq", Scalar test), ("solution", Scalar solution)]) = equationTest test solution 
decodeTest (Mapping [("ex", Scalar eq)]) = expressionTest expr solution
																							where [expr, solution] = splitEquation eq
decodeTest x = TestCase $ assertEqual ("Incorrect test: " ++ show x) True False
															
