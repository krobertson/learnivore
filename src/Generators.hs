module Generators
  where
 
import System.Random
import System.IO.Unsafe
import MathStructures
import Expressions
import Equations

randomInt n = unsafePerformIO $ getStdRandom (randomR (1,n))  
randomInteger n = (Nullary (Integ (fromInteger $ randomInt n)))
randomVariable n = (Nullary (Variable "x"))
randomTerm n = ([randomInteger, randomInteger, randomVariable] !! ((randomInt 3) - 1) $ n)
addition fn n = Binary Add (fn n) (fn n)
subtraction fn n = Binary Subtract (fn n) (fn n)
multiplication fn n = Binary Multiply (fn n) (fn n)
division fn n = Binary Divide (fn n) (fn n)
logarithm fn n = Binary Logarithm (fn n) (fn n)
power fn n = Binary Power (fn n) (fn n)

arithmetic = [addition, subtraction, multiplication, division]
prealgebra = [addition, subtraction, multiplication, division, logarithm, power]

exprLayers :: Integer -> Integer -> [(Integer -> Expression) -> Integer -> Expression] -> (Integer -> Expression) -> Expression
exprLayers 0 n generators termFn = termFn n
exprLayers size n generators termFn = (generators !! (randomInt ((length generators) - 1))) (\x -> exprLayers (size - 1) x generators termFn) n

basicExpr = exprLayers 1 20
simpleExpr = exprLayers 2 20
moderateExpr = exprLayers 3 20
complexExpr = exprLayers 4 20
hardExpr = exprLayers 5 20

smallTerm = randomTerm 10
mediumTerm = randomTerm 30
largeTerm = randomTerm 100

makeEquation :: Integer -> [(Integer -> Expression) -> Integer -> Expression] -> Equation
makeEquation 1 fn = Equation (basicExpr fn randomTerm) (basicExpr fn randomInteger)
makeEquation 2 fn = Equation (simpleExpr fn randomTerm) (simpleExpr fn randomInteger)
makeEquation 3 fn = Equation (moderateExpr fn randomTerm) (moderateExpr fn randomInteger)
makeEquation 4 fn = Equation (complexExpr fn randomTerm) (complexExpr fn randomInteger)
makeEquation 5 fn = Equation (hardExpr fn randomTerm) (hardExpr fn randomInteger)

basicEq = makeEquation 1
simpleEq = makeEquation 2
moderateEq = makeEquation 3
complexEq = makeEquation 4
hardEq = makeEquation 5

