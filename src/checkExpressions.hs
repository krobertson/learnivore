import TypeLevel.NaturalNumber
import Data.Eq.Approximate
import MathStructures
import Expressions
import Generators
import Test.QuickCheck

instance Arbitrary Expression where
  arbitrary = generateExpression
  
generateExpression = do let sizes = [basic, simple, moderate, complex, hard]
                        size <- choose (0, (length sizes) - 1)
                        i <- choose (0, (length arithmetic) - 1)
                        return ((map (\x -> (sizes !! size) arithmetic randomInteger) arithmetic) !! i)

type ApproximateDouble = AbsolutelyApproximateValue (Digits Three)

wrapAD :: Double -> ApproximateDouble Double
wrapAD = AbsolutelyApproximateValue
unwrapAD :: ApproximateDouble Double -> Double
unwrapAD = unwrapAbsolutelyApproximateValue
                 
prop_SolveEval ex = (wrapAD $ evaluate $ (\(Solution (Just xs)) -> (last xs)) (solve ex)) == (wrapAD $ evaluate $ ex) -- simplifying an expression should be equivalent to evaluating it
  where types = ex :: Expression