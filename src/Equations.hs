module Equations
(
solveEq,
solveEquation,
printSolvedEquation,
Equations.valid,
equationSize,
equationTransformations
) where
  
import List
import Data.Set
import Data.Graph.AStar
import MathStructures
import Expressions

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

equationTransformations = [isolateVarByMult, isolateVarBySum, 
                     splitProduct, splitDivide, splitSum, splitSubtract, 
                     splitPower, splitLogarithm
                    ] ++ lhsExpressionTransformations ++ rhsExpressionTransformations


lhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> Equation (exmap fn lhs) rhs) expressionTransformations
rhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> Equation lhs (exmap fn rhs)) expressionTransformations

equationSize :: Equation -> Integer
equationSize (Equation lhs rhs) = (expressionSize lhs + expressionSize rhs) - 1

isolateVarByMult :: Equation -> Equation
isolateVarByMult (Equation (Product xs) rhs) = case sort xs of (Variable str):xs -> Equation (Variable str) (Divide (rhs:[Product xs]))
                                                               _ -> Equation (Product xs) rhs
isolateVarByMult equation = equation

isolateVarBySum :: Equation -> Equation
isolateVarBySum (Equation (Sum ((Variable str):xs)) rhs) = Equation (Variable str) (Subtract (rhs:[Sum xs]))
isolateVarBySum equation = equation

splitProduct :: Equation -> Equation
splitProduct (Equation (Product (x:xs)) rhs) = Equation x (Divide (rhs:xs))
splitProduct (Equation lhs (Product (x:xs))) = Equation (Divide (lhs:xs)) x
splitProduct equation = equation

splitDivide :: Equation -> Equation
splitDivide (Equation (Divide (x:xs)) rhs) = Equation x (Product (rhs:xs))
splitDivide (Equation lhs (Divide (x:xs))) = Equation (Product (lhs:xs)) x
splitDivide equation = equation

splitSum :: Equation -> Equation
splitSum (Equation (Sum (x:xs)) rhs) = Equation x (Subtract (rhs:xs))
splitSum (Equation lhs (Sum (x:xs))) = Equation (Subtract (lhs:xs)) x
splitSum equation = equation

splitSubtract :: Equation -> Equation
splitSubtract (Equation (Subtract (x:xs)) rhs) = Equation x (Sum (rhs:xs))
splitSubtract (Equation lhs (Subtract (x:xs))) = Equation (Sum (lhs:xs)) x
splitSubtract equation = equation

splitPower :: Equation -> Equation
splitPower (Equation (Power x expo) rhs) = Equation expo (Logarithm x rhs)
splitPower (Equation lhs (Power x expo)) = Equation (Logarithm x lhs) expo
splitPower equation = equation

splitLogarithm :: Equation -> Equation
splitLogarithm (Equation (Logarithm b x) rhs) = Equation x (Power b rhs)
splitLogarithm (Equation lhs (Logarithm b x)) = Equation (Power b lhs) x
splitLogarithm equation = equation

-- splitLogarithm :: Equation -> Equation
-- splitLogarithm (Equation (Logarithm base expr) rhs) = Equation x (Subtract (rhs:xs))
-- splitLogarithm (Equation lhs (Logarithm base expr) = Equation (Subtract (lhs:xs)) x
-- splitLogarithm equation = equation

validSolution :: SolvedEquation -> Bool
validSolution (SolvedEquation Nothing) = False
validSolution (SolvedEquation (Just xs)) = Equations.valid . last $ xs 

solvedString :: SolvedEquation -> String
solvedString solvedEquation
          | validSolution solvedEquation = "=> Equation is True!"
          | otherwise = "=> Equation is False!"

solveEquation :: String -> String
solveEquation = processEquation solveEq

printSolvedEquation :: String -> IO ()
printSolvedEquation = putStrLn . processEquation (\ans -> do let xs = solveEq $ ans
                                                             show xs ++ "\n" ++ solvedString xs)
            

valid :: Equation -> Bool
valid (Equation (Variable _) (Constant _)) = True
valid (Equation (Variable _) (Integ _)) = True
valid (Equation lhs rhs) = (evaluate lhs) == (evaluate rhs)