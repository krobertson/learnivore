module Expressions
(
exprSolution,
solve,
evaluate,
expressionSize,
exmap,
expressionTransformations,
solveExpression,
printSolvedExpression,
printExpression,
singleVariable,
constSolved,
simplify
) where
  
import List
import Data.Set
import AStar
import ReadAndWriteMathStructures
import MathStructures 
import AlgebraicStructures
import Matching
import Maybe

solve :: Expression -> Solution
solve expression = Solution (case solutionPath of 
                                  (Just (path, steps)) -> Just ((expression:(reverse path)), "Initial Expression":(reverse steps))
                                  Nothing -> Nothing)
                     where solutionPath = if solved expression 
                                          then Just ([expression], ["Final Expression"]) 
                                          else foldl (\(Just (exprs, strs)) (str,expr) -> (Just (expr:exprs, str:strs)))
                                                     (Just ([],[]))
                                                     (fromMaybe [] $ aStar (expressionGraph . snd)
                                                        (\x y -> 1) 
                                                        (expressionSize . snd)
                                                        (solved . snd) 
                                                        ("Initial Expression", expression))
                          
  

solveExpression :: String -> String
solveExpression = processExpression (show . solve)

exprSolution :: String -> String
exprSolution = processExpression getExprSolution

getExprSolution x = case (solve x) of
                         Solution (Just sol) -> show (last . fst $  sol)
                         Solution (Nothing) -> "No Solution"

simplify x = case (solve x) of
              Solution (Just sol) -> (last . fst $  sol)
              Solution (Nothing) -> x

printSolvedExpression :: String -> IO ()
printSolvedExpression = putStrLn . solveExpression

expand :: Expression -> [(String, Expression)]
expand = twiddle $ List.map exmap expressionTransformations

expressionGraph :: Expression -> Set (String, Expression)
expressionGraph = fromList . expand

twiddle :: [Expression -> [(String, Expression)]] -> Expression -> [(String, Expression)]
twiddle transforms expression = if (not . solved $ expression) 
                                then nub . List.filter (not . (== expression) . snd) $
                                     List.concat $ List.map ($ expression) transforms
                                else []

expressionSize :: Expression -> Integer
expressionSize (Nullary term) = 1
expressionSize (Unary operator expr) = 1 + expressionSize expr
expressionSize (Binary operator leftExpr rightExpr) = sum [1, expressionSize leftExpr, expressionSize rightExpr]

-- solution checkers
solved :: Expression -> Bool
solved (Unary Absolute x)
        | isVarExpr x = varSolved x
        | otherwise = False
solved x = (constSolved x) || (varSolved x)

varSolved :: Expression -> Bool
varSolved (Nullary x) = True
varSolved (Unary _ (Nullary (Variable _))) = True
varSolved (Binary _ x y)
          | isVarExpr x && isVarExpr y && x /= y = False
          | (isVarExpr x && constSolved y) || (constSolved x && isVarExpr y) = True
          | otherwise = False
varSolved _ = False

constSolved :: Expression -> Bool
constSolved (Nullary x) = isNum x
constSolved (Unary Negate (Nullary x)) = isNum x
constSolved _ = False

-- expressionTransformations
expressionTransformations = axiomTransforms ++ [
                             ("Applying A Known Operator to Basic Numbers", transform ev),
                             ("Swapping Commutative Order", commute), ("Swapping Associative Evaluation Order", associate)
                            ]

transform :: (Expression -> Expression) -> (Expression -> [Expression])
transform fn = propagate ((:[]) . fn)

axioms = [  ("Negation", "-(-x)=x"), 
            ("Subtraction equals Addition of a Negative", "x-y=x+(-y)"),
            ("Factoring", "(a*x)+(b*x)=(a+b)*x"), 
            ("Factoring", "x+x=2*x"), 
            ("Factoring", "n*x+x=(n+1)*x"),
            ("Factoring", "a*x+b*y+c*z=a*(x+y*(b/a)+z*(c/a)"), 
            ("Adding Zero", "x+0=x"), 
            ("Multiplying by Zero", "x*0=0"),
            ("Dividing a Zero", "0/x=0"), 
            ("Multiplying two Negatives", "-x*(-y)=x*y"), 
            ("Cancellative Property of Division", "(x*y)/x=y"),
            ("Division equals Multiplication by an inverse", "x/y=x*(1/y)"),
            ("Multiplying by One", "x*1=x"), 
            ("Distributive Law of Multiplication", "x*(y+z)=x*y+x*z"),
            ("Exponentiatiation by Zero", "x^0=1"), 
            ("Exponentiating Zero", "0^x=0"), 
            ("Exponentiating One", "1^x=1"),
            ("Negative Exponentiation", "x^(-y)=1/(x^y)"), 
            ("Exponential Multiplication", "(x^n)*(x^m)=x^(n+m)"),
            ("Exponentiation", "(x^y)*x=x^(y+1)"), 
            ("Definition of squaring", "x*x=x^2"),
            ("FOIL", "(a+b)*(c+d)=a*c+a*d+b*c+b*d"), 
            ("Linearity of the Logarithm", "log<x>(y*z)=log<x>(y)+log<x>(z)"),
            ("Inverse Law of Logarithms", "log<x>(x^y)=y"), 
            ("Inverse Law of Powers", "x^(log<x>(y))=y"),
            ("Completing the Square", "(x^2)+b*x+c=(x+b/2)^2+c-((b/2)^2)")
          ] ++
          [ 
            ("Pythagorean Trig Identity", "sin(u)^2+cos(u)^2=1"),
            ("Pythagorean Trig Identity", "sec(u)^2-tan(u)^2=1"),
            ("Pythagorean Trig Identity", "csc(u)^2-cot(u)^2 = 1"),
            ("Trig Quotient Identity", "sin(u)/cos(u)=tan(u)"),
            ("Trig Quotient Identity", "cos(u)/sin(u)=cot(u)"),
            ("Trig Co-Function Identity", "sin(pi/2 - u)=cos(u)"),
            ("Trig Co-Function Identity", "cos(pi/2 - u)=sin(u)"),
            ("Trig Co-Function Identity", "tan(pi/2 - u)=cot(u)"),
            ("Trig Co-Function Identity", "csc(pi/2 - u)=sec(u)"),
            ("Trig Co-Function Identity", "sec(pi/2 - u)=csc(u)"),
            ("Trig Co-Function Identity", "cot(pi/2 - u)=tan(u)"),
            ("Trig Even-Odd Identity", "sin(-u)=-sin(u)"),
            ("Trig Even-Odd Identity", "cos(-u)=-cos(u)"),
            ("Trig Even-Odd Identity", "tan(-u)=-tan(u)"),
            ("Trig Even-Odd Identity", "sec(-u)=-sec(u)"),
            ("Trig Even-Odd Identity", "csc(-u)=-csc(u)"),
            ("Trig Even-Odd Identity", "cot(-u)=-cot(u)"),
            ("Trig Sum-Difference Formula", "sin(u+v)=sin(u)*cos(v)+cos(u)*sin(v)"),
            ("Trig Sum-Difference Formula", "cos(u+v)=cos(u)*cos(v)+sin(u)*sin(v)"),
            ("Trig Sum-Difference Formula", "tan(u+v)=(tan(u)*tan(v))/(1-tan(u)*tan(v))"),
            ("Trig Double-Angle Formula", "sin(2*u)=2*sin(u)*cos(u)"),
            ("Trig Double-Angle Formula", "cos(2*u)=cos(u)^2-sin(u)^2"),
            ("Trig Double-Angle Formula", "tan(2*u)=(2*tan(u)) / (1-tan(u)^2)"),
            ("Trig Half-Angle Formula", "sin(u)^2=(1-cos(2*u)) / 2"),
            ("Trig Half-Angle Formula", "cos(u)^2=(1+cos(2*u)) / 2"),
            ("Trig Half-Angle Formula", "tan(u)^2=(1-cos(2*u)) / (1+cos(2*u))"),
            ("Trig Sum-To-Product Formula", "sin(u)+sin(v)=2*sin((u+v)/2)*cos((u-v)/2)"),
            ("Trig Sum-To-Product Formula", "sin(u)-sin(v)=2*cos((u+v)/2)*sin((u-v)/2)"),
            ("Trig Sum-To-Product Formula", "cos(u)+cos(v)=2*cos((u+v)/2)*cos((u-v)/2)"),
            ("Trig Sum-To-Product Formula", "cos(u)-cos(v)=-2*sin((u+v)/2)*sin((u-v)/2)"),
            ("Trig Product-To-Sum Formula", "sin(u)*sin(v)=(1/2)*(cos(u-v)-cos(u+v))"),
            ("Trig Product-To-Sum Formula", "cos(u)*cos(v)=(1/2)*(cos(u-v)+cos(u+v))"),
            ("Trig Product-To-Sum Formula", "sin(u)*cos(v)=(1/2)*(sin(u+v)+sin(u-v))"),
            ("Trig Product-To-Sum Formula", "cos(u)*sin(v)=(1/2)*(sin(u+v)-sin(u-v))")
          ] 

axiomTransforms = List.map (\(str, eqn) -> (str, propagate (transEq eqn))) axioms

-- helpers

isNum :: Term -> Bool
isNum (Integ _) = True
isNum (Constant _) = True
isNum x = False

isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

exprIs :: (Term -> Bool) -> Expression -> Bool
exprIs fn (Nullary x) = fn x
exprIs _ _ = False

isVarExpr :: Expression -> Bool
isVarExpr = exprIs isVariable

singleVariable :: Expression -> Bool
singleVariable (Nullary (Variable _)) = True
singleVariable _ = False

-- useful for quickchecking that solutions found through search equal solutions found through straight evaluation

evaluate :: Expression -> Maybe Double
evaluate ex@(Nullary x)                        = value x
evaluate ex@(Unary op v)             = do x <- evaluate v
                                          v <- value . ev $ Unary op (val x)
                                          Just v
evaluate ex@(Binary op lex ler)      = do x <- evaluate lex
                                          y <- evaluate ler
                                          value . ev $ (Binary op (val x) (val y))