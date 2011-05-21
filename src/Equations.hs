module Equations
(
searchEq,
solveEq,
solveEquation,
equivalentEquations,
solveThroughEquations,
solveThroughEqs,
eqnSolution,
printSolvedEquation,
printEquation,
Equations.valid,
equationSize,
equationTransformations,
answer
) where

import List
import Data.Set
import Data.Graph.AStar
import MathStructures
import Expressions

-- echo x = trace (show x) x

equivalentEquations :: String -> String -> String
equivalentEquations inp = processEquation (show . answer . equateEq (parseEquation inp))

solveEquation :: String -> String
solveEquation = processEquation (show . solveEq)

eqnSolution :: String -> String
eqnSolution = processEquation getEqnSolution

solveThroughEquations :: [String] -> String -> String
solveThroughEquations eqs eq = show . solveThroughEqs (List.map parseEquation eqs) $ parseEquation eq

answer :: SolvedEquation -> Equation
answer (SolvedEquation x) = case x of
                                 (Just xs) -> snd. last $ xs
                                 (Nothing) -> Equation (Nullary (Integ 1)) (Nullary (Integ 0))

onThePath :: Equation -> Equation -> Bool
onThePath x y = x == answer (equateEq x y)

searchEq :: (Equation -> Bool) -> (Equation -> Integer) -> Equation -> SolvedEquation
searchEq goalTest heuristicFn equation = SolvedEquation (case solutionPath of (Just path) -> Just (("Initial Equation", equation):path)
                                                                              Nothing -> Nothing)
                                                              where solutionPath = if goalTest equation 
                                                                                   then Just [("Final Equation", equation)] 
                                                                                   else aStar (equationGraph . snd) 
                                                                                              (\x y -> 1) 
                                                                                              (heuristicFn . snd)
                                                                                              (goalTest . snd)
                                                                                              ("Initial Equation", equation)
                                      
solveEq :: Equation -> SolvedEquation                                                     
solveEq = searchEq solvedEq equationSize

solveThroughEqs :: [Equation] -> Equation -> SolvedEquation
solveThroughEqs [] eq = solveEq eq
solveThroughEqs eqs eq = if onThePath (head eqs) eq 
                         then solveThroughEqs (tail eqs) (head eqs)
                         else SolvedEquation Nothing
                         
equateEq :: Equation -> Equation -> SolvedEquation
equateEq equation = searchEq (\x -> x == equation) (\x -> abs (equationSize x - equationSize equation))

join :: String -> [String] -> String
join str = concat . intersperse str
                                                     
solvedEq :: Equation -> Bool
solvedEq (Equation lhs rhs)
        | (singleVariable lhs) && (constSolved rhs) = True
        | constSolved lhs && constSolved rhs = True
        | otherwise = False
        
equationGraph :: Equation -> Set (String, Equation)
equationGraph = fromList . expandEq

expandEq :: Equation -> [(String, Equation)]
expandEq = twiddleEq equationTransformations

namedEqApply :: (String, (Equation -> [Equation])) -> Equation -> [(String, Equation)]
namedEqApply fn eq = List.map (\x -> (fst fn, x)) (snd fn $ eq)

twiddleEq :: [(String, Equation -> [Equation])] -> Equation -> [(String, Equation)]
twiddleEq transforms equation = if (not . solvedEq $ equation) 
                                then List.filter (not . (== equation) . snd) $
                                     List.concat $ List.map (\fn -> namedEqApply fn equation)  transforms
                                else []
                              
equationSize :: Equation -> Integer
equationSize (Equation lhs rhs) = (expressionSize lhs + expressionSize rhs)

getEqnSolution :: Equation -> String
getEqnSolution x = case (solveEq x) of
                        SolvedEquation (Just sol) -> show (snd . last $ sol)
                        SolvedEquation (Nothing) -> "No Solution"

printSolvedEquation :: String -> IO ()
printSolvedEquation = putStrLn . processEquation (\ans -> do let xs = solveEq $ ans
                                                             show xs ++ "\n" ++ solvedString xs)
-- Equation Transformations

equationTransformations = lhsExpressionTransformations ++ rhsExpressionTransformations ++
                         [("Swapping Sides", rotate), ("Splitting Multiplication", splitMultiply), 
                          ("Splitting Division", splitDivide), ("Splitting Addition", splitAdd), 
                          ("Splitting Subtraction", splitSubtract), ("Splitting Exponentiation", splitPowerRight), 
                          ("Splitting Logarithms", splitLogarithmRight), ("Splitting Exponentiation", splitPowerLeft), 
                          ("Spitting Logarithms", splitLogarithmLeft), ("Taking the Root", nthRootBothSidesR), ("Taking the Root", nthRootBothSidesL), 
                          ("Splitting an Nth Root", splitRootLeft), ("Splitting an Nth Root", splitRootRight), 
                          ("Subtracting from one side", shiftSubtract), ("Dividing from one side", shiftDivide)]
                          

  -- (lifted expression transformations)
lhsExpressionTransformations = List.map liftExprTransform expressionTransformations
rhsExpressionTransformations = List.map liftExprTransform expressionTransformations

liftExprTransform :: (String, Expression -> Expression) -> (String, Equation -> [Equation])
liftExprTransform fn = (fst fn, (\eq@(Equation lhs rhs) -> let (ls, rs) = ((exmap fn lhs), (exmap fn rhs))
                                                           in (List.map (\x -> Equation lhs (snd x)) rs) ++
                                                              (List.map (\x -> Equation (snd x) rhs) ls)))

splitMultiply = invert Multiply Divide
splitAdd = invert Add Subtract
splitDivide = invert' Divide Multiply
splitSubtract = invert' Subtract Add

splitPowerLeft = invertPowersLeft Power Logarithm
splitPowerRight = invertPowersRight Power Logarithm

splitRootLeft (Equation (Binary NthRoot n x) rhs) = [Equation x (Binary Power rhs n)]
splitRootLeft equation = [equation]

splitRootRight (Equation lhs (Binary NthRoot n x)) = [Equation (Binary Power lhs n) x]
splitRootRight equation = [equation]

splitLogarithmLeft (Equation (Binary Logarithm base x) rhs) = [Equation x (Binary Power base rhs)]
splitLogarithmLeft equation = [equation]

splitLogarithmRight (Equation lhs (Binary Logarithm base x)) = [Equation (Binary Power base lhs) x]
splitLogarithmRight equation = [equation]

nthRootBothSidesR (Equation lhs (Binary Power x n)) = [Equation (Binary NthRoot n lhs) x]
nthRootBothSidesR x = [x]

nthRootBothSidesL (Equation (Binary Power x n) rhs) = [Equation x (Binary NthRoot n rhs)]
nthRootBothSidesL x = [x]

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

rotate :: Equation -> [Equation]
rotate (Equation lhs rhs) = [Equation rhs lhs]

shiftSubtract = shift Subtract (Nullary (Integ 0))
shiftDivide = shift Divide (Nullary (Integ 1))

invert :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invert op inverse eq@(Equation lhs rhs) = invertL ++ invertR
                                           where invertL = (List.map (\x -> Equation (treeify op (ls List.\\ [x])) (Binary inverse rhs x)) ls)
                                                 invertR = (List.map (\x -> Equation (Binary inverse lhs x) (treeify op (rs List.\\ [x]))) rs)
                                                 ls = topLevelExprs op lhs
                                                 rs = topLevelExprs op rhs
                                              
invert' :: BinaryOp -> BinaryOp -> Equation -> [Equation]
invert' op inverse eq@(Equation lhs rhs) = invertL' ++ invertR'
                                            where invertL' = if ls == [] then [] else (List.map (\x -> Equation (treeify op (l:((tail ls) List.\\ [x]))) (Binary inverse rhs x)) $ tail ls)
                                                  invertR' = if rs == [] then [] else (List.map (\x -> Equation (Binary inverse lhs x) (treeify op (r:((tail rs) List.\\ [x])))) $ tail rs)
                                                  ls = topLevelExprs op lhs
                                                  rs = topLevelExprs op rhs
                                                  l = head ls
                                                  r = head rs

shift :: BinaryOp -> Expression -> Equation -> [Equation]
shift op unit (Equation lhs rhs) = [Equation unit (Binary op rhs lhs), Equation (Binary op lhs rhs) unit]

-- Validity
valid :: Equation -> Bool
valid (Equation (Nullary (Variable _)) (Nullary (Constant _))) = True
valid (Equation (Nullary (Variable _)) (Nullary (Integ _))) = True
valid (Equation lhs rhs) = (evaluate lhs) == (evaluate rhs)

validSolution :: SolvedEquation -> Bool
validSolution (SolvedEquation Nothing) = False
validSolution (SolvedEquation (Just xs)) = Equations.valid . snd . last $ xs 

solvedString :: SolvedEquation -> String
solvedString solvedEquation
          | validSolution solvedEquation = "=> Equation is True!"
          | otherwise = "=> Equation is False!"