import System.Random
import System.IO.Unsafe
import MathStructures
import Test.QuickCheck

-- newtype ExprString = ExprString String
-- instance Arbitrary ExprString where
--   arbitrary = do i <- choose (2, 20)
--                  terms <- map (\fn -> fn(1)) $ take (i+1) (repeat arbitraryTerm)
--                  ops <- map (\fn -> fn(1)) $ take (i) (repeat arbitraryOperator)
--                  return ((head terms):(zipWith (:) (tail terms) ops))
--   
--   
-- randomInt n = unsafePerformIO $ getStdRandom (randomR (1,n))
-- 
-- arbitraryInt = show . randomInt $ 20
-- arbitraryConst = (show (fromInteger $ randomInt 20))
-- arbitraryVariable = "x"
-- terms = [arbitraryInt, arbitraryConst, arbitraryVariable]
-- operators = "+*/^-"
-- arbitraryTerm _ = (terms !! (randomInt $ (length terms) - 1))
-- arbitraryOperator _ = (operators !! (randomInt $ (length operators) - 1))


-- it should recognize a single int, const, or variable as an expression
-- prop_SingleInt inp = processExpression id inp == show (Integ (fromInteger . read $ inp)) 
-- prop_SingleConst inp = processExpression id inp == show (Constant (read inp))
-- prop_SingleVar inp = processExpression id inp == show (Variable inp)
-- 
-- prop_SingleInt "1"
-- prop_SingleConst "2.0"
-- prop_SingleVar "x"

-- it should recognize any negated term as an expression

-- it should recognize any interleaved combination of terms and operators as an expression provided it starts and stops with a term
