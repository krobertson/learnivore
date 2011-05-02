import System.Random
import System.IO.Unsafe
import MathStructures
import Test.QuickCheck



instance Arbitrary String where
  arbitrary = do i <- choose (2, 20)
                 term:terms <- map (\fn -> fn()) $ take (i+1) arbitraryTerm
                 ops <- map (\fn -> fn()) $ take (i) arbitraryOperator
                 return term:(zipWith (:) terms ops)
  
  
randomInt n = unsafePerformIO $ getStdRandom (randomR (1,n))

arbitraryInt = show . randomInt $ 20
arbitraryConst = return (show (fromInteger $ randomInt 20))
arbitraryVariable = return "x"
terms = [arbitraryInt, arbitraryConst, arbitraryVariable]
operators = "+*/^-"
arbitraryTerm = do i <- choose (0, (length terms) - 1)
                   return (terms !! i)
arbitraryOperator = do i <- choose (0, (length operators) - 1)
                       return (operators !! i)


-- it should recognize a single int, const, or variable as an expression

-- it should recognize any negated term as an expression

-- it should recognize any interleaved combination of terms and operators as an expression provided it starts and stops with a term
