module Generators
  where
 
import System.Random
import System.IO.Unsafe
import MathStructures
import Equations   

randomInt n = unsafePerformIO $ getStdRandom (randomR (1,n))  
randomInteger n = Integ (fromInteger $ randomInt n)
randomVariable n = Variable "x"
randomTerm n = [randomInteger, randomInteger, randomVariable] !! ((randomInt 3) - 1) $ n 
addition fn n = Sum $ [fn n, fn n]
subtraction fn n = Subtract $ [fn n, fn n]
multiplication fn n = Product $ [fn n, fn n]
division fn n = Divide $ [fn n, fn n]
logarithm fn n = Logarithm (fn n) (fn n)
power fn n = Power (fn n) (fn n)

arithmetic = [addition, subtraction, multiplication, division]
prealgebra = [addition, subtraction, multiplication, division, logarithm, power]

layers :: Integer -> Integer -> [(Integer -> Expression) -> Integer -> Expression] -> (Integer -> Expression) -> Expression
layers 0 n generators termFn = termFn n
layers size n generators termFn = (generators !! (randomInt ((length generators) - 1))) (\x -> layers (size - 1) x generators termFn) n

basic = layers 1 20
simple = layers 2 20
moderate = layers 3 20
complex = layers 4 20
hard = layers 5 20
