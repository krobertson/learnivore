module Equations
(
searchEq,
solveEq,
solveEquation,
equivalentEquations,
eqnSolution,
printSolvedEquation,
printEquation,
Equations.valid,
equationSize,
equationTransformations,
) where
  
import List
import Data.Set
import Data.Graph.AStar
import MathStructures
import Expressions

equivalentEquations :: String -> String -> String
equivalentEquations inp = processEquation (show . answer . equateEq (parseEquation inp))

solveEquation :: String -> String
solveEquation = processEquation (show . solveEq)

eqnSolution :: String -> String
eqnSolution = processEquation getEqnSolution

answer :: SolvedEquation -> Equation
answer (SolvedEquation x) = case x of
                                  (Just xs) -> last xs
                                  (Nothing) -> Equation (Nullary (Integ 1)) (Nullary (Integ 0))

onThePath :: Equation -> Equation -> Bool
onThePath x y = x == answer (equateEq x y)

searchEq :: (Equation -> Bool) -> (Equation -> Integer) -> Equation -> SolvedEquation
searchEq goalTest heuristicFn equation = SolvedEquation (case solutionPath of (Just path) -> Just (equation:path)
                                                                              Nothing -> Nothing)
                                                              where solutionPath = if goalTest equation 
                                                                                   then Just [equation] 
                                                                                   else aStar equationGraph 
                                                                                              (\x y -> 1) 
                                                                                              heuristicFn
                                                                                              goalTest
                                                                                              equation
                                      
solveEq :: Equation -> SolvedEquation                                                     
solveEq = searchEq solvedEq equationSize
equateEq :: Equation -> Equation -> SolvedEquation
equateEq equation = searchEq (\x -> x == equation) (\x -> abs (equationSize x - equationSize equation))

join :: String -> [String] -> String
join str = concat . intersperse str
                                                     
solvedEq :: Equation -> Bool
solvedEq (Equation lhs rhs)
        | (singleVariable lhs) && (constSolved rhs) = True
        | constSolved lhs && constSolved rhs = True
        | otherwise = False
        
equationGraph :: Equation -> Set Equation
equationGraph = fromList . expandEq

expandEq :: Equation -> [Equation]
expandEq = twiddleEq equationTransformations

twiddleEq :: [Equation -> [Equation]] -> Equation -> [Equation]
twiddleEq transforms equation = if (not . solvedEq $ equation) 
                                then List.filter (not . (== equation)) $
                                     List.concat $ List.map ($ equation) transforms
                                else []
                              
equationSize :: Equation -> Integer
equationSize (Equation lhs rhs) = (expressionSize lhs + expressionSize rhs)

liftExprTLeft :: (Expression -> Expression) -> Equation -> [Equation]
liftExprTLeft fn (Equation lhs rhs) = List.map (\x -> Equation x rhs) (exmap fn lhs)

liftExprTRight :: (Expression -> Expression) -> Equation -> [Equation]
liftExprTRight fn (Equation lhs rhs) = List.map (\x -> Equation lhs x) (exmap fn rhs)

getEqnSolution :: Equation -> String
getEqnSolution x = case (solveEq x) of
                        SolvedEquation (Just sol) -> show (last sol)
                        SolvedEquation (Nothing) -> "No Solution"

printSolvedEquation :: String -> IO ()
printSolvedEquation = putStrLn . processEquation (\ans -> do let xs = solveEq $ ans
                                                             show xs ++ "\n" ++ solvedString xs)
-- Equation Transformations

equationTransformations = lhsExpressionTransformations ++ rhsExpressionTransformations ++
                         [rotate, multByOneLeft, multByOneRight, addZeroLeft, addZeroRight, splitMultiply, 
                          splitMultiply', splitDivide, splitAdd, splitAdd', splitSubtract,
                          splitPowerRight, splitLogarithmRight, splitPowerLeft, splitLogarithmLeft,
                          splitPowerRoot, subFromLeft, subFromRight, 
                          splitRootLeft, splitRootRight, divFromLeft, divFromRight]
                          

  -- (lifted expression transformations)
lhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> List.map (\x -> Equation x rhs) (exmap fn lhs)) expressionTransformations
rhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> List.map (\x -> Equation lhs x) (exmap fn rhs)) expressionTransformations

splitMultiply = invert Multiply Divide
splitMultiply' = invert' Multiply Divide
splitAdd = invert Add Subtract
splitAdd' = invert' Add Subtract
splitDivide = invert Divide Multiply
splitSubtract = invert Subtract Add

splitPowerLeft = invertPowersLeft Power Logarithm
splitPowerRight = invertPowersRight Power Logarithm
splitPowerRoot = invert Power NthRoot

invertPowersRight :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertPowersRight op inverse equation@(Equation (Binary op1 x y) rhs)
                  | op == op1 = [Equation y (Binary inverse x rhs)]
                  | otherwise = [equation]
invertPowersRight _ _ equation = [equation]

invertPowersLeft :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertPowersLeft op inverse equation@(Equation lhs (Binary op1 x y))
                 | op == op1 = [Equation (Binary inverse x lhs) y]
                 | otherwise = [equation]
invertPowersLeft _ _ equation = [equation]

splitRootLeft (Equation (Binary NthRoot n x) rhs) = [Equation x (Binary Power rhs n)]
splitRootLeft equation = [equation]

splitRootRight (Equation lhs (Binary NthRoot n x)) = [Equation (Binary Power lhs n) x]
splitRootRight equation = [equation]

splitLogarithmLeft (Equation (Binary Logarithm base x) rhs) = [Equation x (Binary Power base rhs)]
splitLogarithmLeft equation = [equation]

splitLogarithmRight (Equation lhs (Binary Logarithm base x)) = [Equation (Binary Power base lhs) x]
splitLogarithmRight equation = [equation]

rotate :: Equation -> [Equation]
rotate (Equation lhs rhs) = [Equation rhs lhs]

subFromLeft = toRight Subtract (Nullary (Integ 0))
subFromRight = toLeft Subtract (Nullary (Integ 0))
divFromLeft = toRight Divide (Nullary (Integ 1))
divFromRight = toLeft Multiply (Nullary (Integ 1))

multByOneLeft = liftExprTLeft (\x -> (Binary Multiply (Nullary (Integ 1)) x))
multByOneRight = liftExprTRight (\x -> (Binary Multiply (Nullary (Integ 1)) x))
addZeroLeft = liftExprTLeft (\x -> (Binary Add (Nullary (Integ 0)) x))
addZeroRight = liftExprTRight (\x -> (Binary Add (Nullary (Integ 0)) x))

invert :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invert op inverse (Equation lhs@(Binary op2 xl yl) rhs@(Binary op3 xr yr))
       | op == op2 && op2 == op3 = [Equation (Binary inverse lhs yr) xr, Equation xl (Binary inverse rhs yl)]
       | otherwise = [Equation lhs rhs]
invert op inverse equation@(Equation lhs (Binary op2 x y))
       | op == op2 = [Equation (Binary inverse lhs y) x]
       | otherwise = [equation]
invert op inverse equation@(Equation lhs@(Binary op2 x y) rhs)
       | op == op2 = [Equation x (Binary inverse rhs y)]
       | otherwise = [equation]
invert _ _ eqn = [eqn]

invert' :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invert' op inverse (Equation lhs@(Binary op2 xl yl) rhs@(Binary op3 xr yr))
        | op == op2 && op2 == op3 = [Equation (Binary inverse lhs xr) yr, Equation yl (Binary inverse rhs xl)]
        | otherwise = [Equation lhs rhs]
invert' op inverse equation@(Equation lhs (Binary op2 x y))
        | op == op2 = [Equation (Binary inverse lhs x) y]
        | otherwise = [equation]
invert' op inverse equation@(Equation (Binary op2 x y) rhs)
        | op == op2 = [Equation y (Binary inverse rhs x)]
        | otherwise = [equation]
invert' _ _ eqn = [eqn]

toRight :: BinaryOp -> Expression -> Equation -> [Equation]
toRight op unit (Equation lhs rhs) = [(Equation unit (Binary op rhs lhs))]

toLeft :: BinaryOp -> Expression -> Equation -> [Equation]
toLeft op unit (Equation lhs rhs) = [(Equation (Binary op lhs rhs) unit)]

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