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
isOperation,
isTerm,
simplify
) where
  
import List
import Data.Set
import AStar
import ReadAndWriteMathStructures
import MathStructures 
import AlgebraicStructures
import Matching

fromMaybe (Just x) = x
fromMaybe Nothing = []

solve :: Expression -> Solution
solve expression = Solution (case solutionPath of (Just (path, steps)) -> Just ((expression:(reverse path)), "Initial Expression":(reverse steps))
                                                  Nothing -> Nothing)
                     where solutionPath = if solved expression 
                                          then Just ([expression], ["Final Expression"]) 
                                          else foldl (\(Just (exprs, strs)) (str,expr) -> (Just (expr:exprs, str:strs)))
													(Just ([],[]))
													(fromMaybe $ aStar (expressionGraph . snd)
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
                             ("Applying A Known Operator to Basic Numbers", evalexpr),
                             ("Swapping Commutative Order", commute), ("Swapping Associative Evaluation Order", associate)
                            ]

transform :: (Expression -> Expression) -> (Expression -> [Expression])
transform fn = propagate ((:[]) . fn)

axioms = [  ("Negation", "-(-x)=x"), ("Subtraction equals Addition of a Negative", "x-y=x+(-y)"),
            ("Factoring", "(a*x)+(b*x)=(a+b)*x"), ("Factoring", "x+x=2*x"), ("Factoring", "n*x+x=(n+1)*x"),
            ("Factoring", "a*x+b*y+c*z=a*(x+y*(b/a)+z*(c/a)"), ("Adding Zero", "x+0=x"), ("Multiplying by Zero", "x*0=0"),
            ("Dividing a Zero", "0/x=0"), ("Multiplying two Negatives", "-x*(-y)=x*y"), ("Cancellative Property of Division", "(x*y)/x=y"),
            ("Division equals Multiplication by an inverse", "x/y=x*(1/y)"),
            ("Multiplying by One", "x*1=x"), ("Distributive Law of Multiplication", "x*(y+z)=x*y+x*z"),
            ("Exponentiatiation by Zero", "x^0=1"), ("Exponentiating Zero", "0^x=0"), ("Exponentiating One", "1^x=1"),
            ("Negative Exponentiation", "x^(-y)=1/(x^y)"), ("Exponential Multiplication", "(x^n)*(x^m)=x^(n+m)"),
            ("Exponentiation", "(x^y)*x=x^(y+1)"), ("Definition of squaring", "x*x=x^2"),
            ("FOIL", "(a+b)*(c+d)=a*c+a*d+b*c+b*d"), ("Linearity of the Logarithm", "log<x>(y*z)=log<x>(y)+log<x>(z)"),
            ("Inverse Law of Logarithms", "log<x>(x^y)=y"), ("Inverse Law of Powers", "x^(log<x>(y))=y"),
            ("Completing the Square", "(x^2)+b*x+c=(x+b/2)^2+c-((b/2)^2)")
          ]

axiomTransforms = List.map (\(str, eqn) -> (str, propagate (transEq eqn))) axioms

evalexpr = transform ev

-- helpers

eitherOr :: (Expression -> Bool) -> Expression -> Expression -> Bool
eitherOr fn x y = fn x || fn y

isNum :: Term -> Bool
isNum (Integ _) = True
isNum (Constant _) = True
isNum x = False

isZero :: Term -> Bool
isZero (Integ 0) = True
isZero (Constant 0.0) = True
isZero _ = False

isOne :: Term -> Bool
isOne (Integ 1) = True
isOne (Constant 1.0) = True
isOne _ = False

isOperation :: BinaryOp -> Expression -> Bool
isOperation op1 (Binary op2 _ _) = op1 == op2
isOperation _ _ = False

isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

isTerm :: Expression -> Bool
isTerm (Nullary _) = True
isTerm (Unary Negate (Nullary _)) = True
isTerm _ = False

exprIs :: (Term -> Bool) -> Expression -> Bool
exprIs fn (Nullary x) = fn x
exprIs _ _ = False

isNumExpr (Unary Negate x) = (exprIs isNum x)
isNumExpr x = (exprIs isNum x)  

isZeroExpr :: Expression -> Bool
isZeroExpr = exprIs isZero

isOneExpr :: Expression -> Bool
isOneExpr = exprIs isOne

isVarExpr :: Expression -> Bool
isVarExpr = exprIs isVariable

isVariableProduct :: Expression -> Bool
isVariableProduct (Binary Multiply (Nullary x) (Nullary y))
                  | isVariable x && isNum y = True
                  | isVariable y && isNum x = True
isVariableProduct _ = False

getConstant :: Expression -> Expression
getConstant (Binary Multiply (Nullary x) (Nullary y))
 | isVariable x && isNum y = (Nullary y)
 | isVariable y && isNum x = (Nullary x)
 | otherwise = val 0
getConstant (Nullary (Variable x)) = val 1
getConstant _ = val 0

singleVariable :: Expression -> Bool
singleVariable (Nullary (Variable _)) = True
singleVariable _ = False

variable :: Expression -> String
variable (Nullary (Variable str)) = str
variable _ = ""

-- useful for quickchecking that solutions found through search equal solutions found through straight evaluation

evaluate :: Expression -> Maybe Double
evaluate ex@(Nullary x)                        = value x
evaluate ex@(Unary op v)             = do x <- evaluate v
                                          v <- value . ev $ Unary op (val x)
                                          Just v
evaluate ex@(Binary op lex ler)      = do x <- evaluate lex
                                          y <- evaluate ler
                                          value . ev $ (Binary op (val x) (val y))