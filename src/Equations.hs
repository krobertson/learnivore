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
splitRootLeft,
splitRootRight
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
                         [rotate, multByOneLeft, multByOneRight, addZeroLeft, addZeroRight, splitMultiplyLeft, splitMultiplyLeft', splitMultiplyRight, splitMultiplyRight',
                          splitDivideLeft, splitAddLeft, splitAddLeft', splitSubtractLeft,
                          splitDivideRight, splitAddRight, splitAddRight', splitSubtractRight,
                          splitPowerRight, splitLogarithmRight, splitPowerLeft, splitLogarithmLeft,
                          splitPowerRootLeft, splitPowerRootRight, subFromLeft, subFromRight, divFromLeft, divFromRight]
                          

  -- (lifted expression transformations)
lhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> List.map (\x -> Equation x rhs) (exmap fn lhs)) expressionTransformations
rhsExpressionTransformations = List.map (\fn (Equation lhs rhs)-> List.map (\x -> Equation lhs x) (exmap fn rhs)) expressionTransformations

splitMultiplyLeft = invertLeft Multiply Divide
splitMultiplyRight = invertRight Multiply Divide
splitMultiplyLeft' = invertLeft' Multiply Divide
splitMultiplyRight' = invertRight' Multiply Divide
splitDivideLeft = invertLeft Divide Multiply
splitDivideRight = invertRight Divide Multiply
splitAddLeft = invertLeft Add Subtract
splitAddRight = invertRight Add Subtract
splitAddLeft' = invertLeft' Add Subtract
splitAddRight' = invertRight' Add Subtract
splitSubtractLeft = invertLeft Subtract Add
splitSubtractRight = invertRight Subtract Add

splitPowerLeft = invertPowersLeft Power Logarithm
splitPowerRight = invertPowersRight Power Logarithm
splitLogarithmLeft = invertPowersLeft Logarithm Power
splitLogarithmRight = invertPowersRight Logarithm Power
splitPowerRootLeft = invertLeft Power NthRoot
splitPowerRootRight = invertRight Power NthRoot

invertPowersRight :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertPowersRight opLeft opRight equation@(Equation (Binary op x y) rhs)
                  | opLeft == op = [Equation y (Binary opRight x rhs)]
                  | otherwise = [equation]
invertPowersRight _ _ equation = [equation]

invertPowersLeft :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertPowersLeft opLeft opRight equation@(Equation lhs (Binary op x y))
                 | opLeft == op = [Equation (Binary opLeft x lhs) y]
                 | otherwise = [equation]
invertPowersLeft _ _ equation = [equation]

splitRootLeft (Equation (Binary NthRoot n x) rhs) = Equation x (Binary Power rhs n)
splitRootLeft equation = equation

splitRootRight (Equation lhs (Binary NthRoot n x)) = Equation (Binary Power lhs n) x
splitRootRight equation = equation

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

invertRight :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertRight op inverse (Equation lhs (Binary op2 x y))
            | op == op2 = [Equation (Binary inverse lhs y) x]
            | otherwise = [Equation lhs (Binary op2 x y)]
invertRight _ _ eqn = [eqn]

invertRight' :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertRight' op inverse (Equation lhs (Binary op2 x y))
             | op == op2 = [Equation (Binary inverse lhs x) y]
             | otherwise = [Equation lhs (Binary op2 x y)]
invertRight' _ _ eqn = [eqn]

invertLeft :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertLeft op inverse (Equation (Binary op2 x y) rhs)
           | op == op2 = [Equation x (Binary inverse rhs y)]
           | otherwise = [Equation (Binary op2 x y) rhs]
invertLeft _ _ eqn = [eqn]

invertLeft' :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invertLeft' op inverse (Equation (Binary op2 x y) rhs)
            | op == op2 = [Equation y (Binary inverse rhs x)]
            | otherwise = [Equation (Binary op2 x y) rhs]
invertLeft' _ _ eqn = [eqn]

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