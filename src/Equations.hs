module Equations
(
solveEq,
solveEquation,
printSolvedEquation,
printEquation,
Equations.valid,
equationSize,
equationTransformations
) where
  
import List
import Data.Set
import Data.Graph.AStar
import MathStructures
import Expressions

solveEquation :: String -> String
solveEquation = processEquation (show . solveEq)

eqnSolution :: String -> String
eqnSolution = processEquation getEqnSolution

getEqnSolution :: Equation -> String
getEqnSolution x = case (solveEq x) of
                        SolvedEquation (Just sol) -> show (last sol)
                        SolvedEquation (Nothing) -> "No Solution"

printSolvedEquation :: String -> IO ()
printSolvedEquation = putStrLn . processEquation (\ans -> do let xs = solveEq $ ans
                                                             show xs ++ "\n" ++ solvedString xs)

solveEq :: Equation -> SolvedEquation
solveEq equation = SolvedEquation (case solutionPath of (Just path) -> Just (equation:path)
                                                        Nothing -> Nothing)
                     where solutionPath = if solvedEq equation 
                                          then Just [equation] 
                                          else aStar equationGraph 
                                                     (\x y -> 1) 
                                                     equationSize
                                                     solvedEq
                                                     equation
                                                     
solvedEq :: Equation -> Bool
solvedEq (Equation lhs rhs)
        | (singleVariable lhs) && (constSolved rhs) = True
        | constSolved lhs && constSolved rhs = True
        | otherwise = False
        
equationGraph :: Equation -> Set Equation
equationGraph = fromList . expandEq

expandEq :: Equation -> [Equation]
expandEq = twiddleEq equationTransformations

twiddleEq :: [Equation -> Equation] -> Equation -> [Equation]
twiddleEq transforms equation = if (not . solvedEq $ equation) 
                                then List.filter (not . (== equation)) $
                                     List.map ($ equation) transforms
                                else []
                              
equationSize :: Equation -> Integer
equationSize (Equation lhs rhs) = (expressionSize lhs + expressionSize rhs) - 1

-- Equation Transformations

equationTransformations = [splitMultiply, splitDivide, splitAdd, splitSubtract, 
                          splitPower, splitLogarithm, addToRight, addToLeft, multToRight, multToLeft] ++ 
                          lhsExpressionTransformations ++ rhsExpressionTransformations

  -- (lifted expression transformations)
lhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> Equation (exmap fn lhs) rhs) expressionTransformations
rhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> Equation lhs (exmap fn rhs)) expressionTransformations

splitMultiply = invert Multiply Divide
splitDivide = invert Divide Multiply
splitAdd = invert Add Subtract
splitSubtract = invert Subtract Add

splitPower :: Equation -> Equation
splitPower (Equation (Binary Power x expo) rhs) = Equation expo (Binary Logarithm x rhs)
splitPower (Equation lhs (Binary Power x expo)) = Equation (Binary Logarithm x lhs) expo
splitPower equation = equation

splitLogarithm :: Equation -> Equation
splitLogarithm (Equation (Binary Logarithm b x) rhs) = Equation x (Binary Power b rhs)
splitLogarithm (Equation lhs (Binary Logarithm b x)) = Equation (Binary Power b lhs) x
splitLogarithm equation = equation

-- splitPower = invert Power Logarithm
-- splitLogarithm = invert Logarithm Power

addToRight = toRight Add (Nullary (Integ 0))
addToLeft = toLeft Add (Nullary (Integ 0))
multToRight = toRight Add (Nullary (Integ 1))
multToLeft = toLeft Add (Nullary (Integ 1))

invert :: BinaryOp -> BinaryOp -> Equation -> Equation
invert op inverse (Equation (Binary op2 x y) rhs)
       | op == op2 = Equation x (Binary inverse rhs y)
       | otherwise = Equation (Binary op2 x y) rhs
invert op inverse (Equation lhs (Binary op2 x y))
       | op == op2 = Equation (Binary inverse lhs y) x
       | otherwise = Equation lhs (Binary op2 x y)
invert _ _ eqn = eqn

toRight :: BinaryOp -> Expression -> Equation -> Equation
toRight op unit (Equation lhs rhs) = (Equation unit (Binary op rhs lhs))

toLeft :: BinaryOp -> Expression -> Equation -> Equation
toLeft op unit (Equation lhs rhs) = (Equation (Binary op lhs rhs) unit)

-- Validity
valid :: Equation -> Bool
valid (Equation (Nullary (Variable _)) (Nullary (Constant _))) = True
valid (Equation (Nullary (Variable _)) (Nullary (Integ _))) = True
valid (Equation lhs rhs) = (evaluate lhs) == (evaluate rhs)

validSolution :: SolvedEquation -> Bool
validSolution (SolvedEquation Nothing) = False
validSolution (SolvedEquation (Just xs)) = Equations.valid . last $ xs 

solvedString :: SolvedEquation -> String
solvedString solvedEquation
          | validSolution solvedEquation = "=> Equation is True!"
          | otherwise = "=> Equation is False!"