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
constSolved
) where
  
import List
import Data.Set
import Data.Graph.AStar
import MathStructures  
      
solve :: Expression -> Solution
solve expression = Solution (case solutionPath of (Just path) -> Just (expression:path)
                                                  Nothing -> Nothing)
                     where solutionPath = if solved expression 
                                          then Just [expression] 
                                          else aStar expressionGraph 
                                                     (\x y -> 1) 
                                                     expressionSize 
                                                     solved 
                                                     expression
  

solveExpression :: String -> String
solveExpression = processExpression (show . solve)

exprSolution :: String -> String
exprSolution = processExpression getExprSolution

getExprSolution x = case (solve x) of
                         Solution (Just sol) -> show (last sol)
                         Solution (Nothing) -> "No Solution"

printSolvedExpression :: String -> IO ()
printSolvedExpression = putStrLn . solveExpression

expand :: Expression -> [Expression]
expand = twiddle $ List.map exmap expressionTransformations

expressionGraph :: Expression -> Set Expression                          
expressionGraph = fromList . expand

twiddle :: [Expression -> Expression] -> Expression -> [Expression]
twiddle transforms expression = if (not . solved $ expression) 
                                then List.filter (not . (== expression)) $
                                     List.map ($ expression) transforms
                                else []

expressionSize :: Expression -> Integer
expressionSize (Nullary term) = 1
expressionSize (Unary operator expr) = 1 + expressionSize expr
expressionSize (Binary operator leftExpr rightExpr) = sum [1, expressionSize leftExpr, expressionSize rightExpr]

-- solution checkers
solved :: Expression -> Bool
solved (Unary Absolute x) = varSolved x
solved x = (constSolved x) || (varSolved x)

varSolved :: Expression -> Bool
varSolved (Nullary (Variable x)) = True
varSolved (Unary _ (Nullary (Variable _))) = True
varSolved (Binary _ x y)
          | isVarExpr x && isVarExpr y && x /= y = False
          | isTerm x && isTerm y= True
          | otherwise = False
varSolved _ = False

constSolved :: Expression -> Bool
constSolved (Nullary (Constant _)) = True
constSolved (Nullary (Integ _)) = True
constSolved _ = False

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

isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

isTerm :: Expression -> Bool
isTerm (Nullary _) = True
isTerm _ = False

exprIs :: (Term -> Bool) -> Expression -> Bool
exprIs fn (Nullary x) = fn x
exprIs _ _ = False

isZeroExpr :: Expression -> Bool
isZeroExpr = exprIs isZero

isOneExpr :: Expression -> Bool
isOneExpr = exprIs isOne

isVarExpr :: Expression -> Bool
isVarExpr = exprIs isVariable

isVariableProduct :: Expression -> Bool
isVariableProduct (Binary Multiply (Nullary x) (Nullary y))
                  | isVariable x && isNum y = True
                  | isVariable y && isNum y = True
isVariableProduct _ = False

singleVariable :: Expression -> Bool
singleVariable (Nullary (Variable _)) = True
singleVariable _ = False

numberOfVariables :: Expression -> Int
numberOfVariables expression = length . listOfVariables $ expression

listOfVariables :: Expression -> [String]
listOfVariables (Nullary (Variable str)) = [str]
listOfVariables (Unary op x) = listOfVariables x
listOfVariables (Binary op x y) = nub . concatMap listOfVariables $ [x,y]
listOfVariables _ = []

-- expressionTransformations
expressionTransformations = [absolutify, addZero, multiplyByZero, multiplyByOne, distribute, 
                            applyAdd, applySub, applyMult, applyDiv, applyLog, applyPow, 
                            pop, negatify]

pop :: Expression -> Expression
pop (Unary Parens x) = x
pop x = x

addZero :: Expression -> Expression
addZero (Binary Add x y) 
        | isZeroExpr x = y
        | isZeroExpr y = x
        | otherwise = (Binary Add x y)
addZero x = x

multiplyByZero :: Expression -> Expression
multiplyByZero (Binary Multiply x y) 
               | isZeroExpr x || isZeroExpr y = Nullary (Integ 0)
               | otherwise = (Binary Multiply x y)
multiplyByZero x = x

multiplyByOne :: Expression -> Expression
multiplyByOne (Binary Multiply x y) 
               | eitherOr isOneExpr x y && isOneExpr x = y
               | eitherOr isOneExpr x y && isOneExpr y = x
               | otherwise = (Binary Multiply x y)
multiplyByOne x = x

absolutify :: Expression -> Expression
absolutify (Unary Absolute (Unary Negate x)) = x
absolutify (Unary Absolute x) = x
absolutify expression = expression

negatify :: Expression -> Expression
negatify (Unary Negate (Nullary (Constant x))) = (Nullary (Constant (negate x)))
negatify (Unary Negate (Nullary (Integ x))) = (Nullary (Integ (negate x)))
negatify x = x

distribute :: Expression -> Expression
distribute (Binary Multiply (Binary Add x y) z) = Binary Add (Binary Multiply x z) (Binary Multiply y z)
distribute x = x

-- functions for applying operators over terms
applyMult = applyBinOp Multiply multTerms
applySub  = applyBinOp Subtract subTerms
applyAdd  = applyBinOp Add addTerms
applyDiv  = applyBinOp Divide divTerms
applyLog  = applyBinOp Logarithm logTerms
applyPow  = applyBinOp Power powTerms

-- apply helpers
forNums :: BinaryOp -> (Double -> Double -> Double) -> Term -> Term -> Expression
forNums _ fn (Integ a) (Integ b) = Nullary (Constant (fn (fromInteger a) (fromInteger b)))
forNums _ fn (Constant x) (Integ y) = Nullary (Constant (fn x (fromInteger y)))
forNums _ fn (Integ x) (Constant y) = Nullary (Constant (fn (fromInteger x) y))
forNums op _ x y = Binary op (Nullary x) (Nullary y)

forVars :: BinaryOp -> (Term -> Expression) -> Term -> Term -> Expression
forVars op fn (Variable x) (Variable y) 
        | x == y = fn (Variable x)
        | otherwise = Binary op (Nullary (Variable x)) (Nullary (Variable y)) 
forVars op _ x y = Binary op (Nullary x) (Nullary y)

forTerms numFn varFn x y
         | isNum x && isNum y = numFn x y
         | otherwise = varFn x y

applyBinOp op fn (Binary op2 (Nullary x) (Nullary y)) 
      | op == op2 = fn x y
      | otherwise = Binary op2 (Nullary x) (Nullary y) 
applyBinOp _ _ x = x

addTerms = forTerms addNums addVars
subTerms = forTerms subNums subVars
multTerms = forTerms multNums multVars
divTerms = forTerms divNums divVars
powTerms = forTerms powNums powVars
logTerms = forTerms logNums logVars

addVars = forVars Add (\x -> Binary Multiply (Nullary (Integ 2)) (Nullary x))
subVars = forVars Subtract (\x -> Nullary (Integ 0))
multVars = forVars Multiply (\x -> Binary Multiply (Nullary (Integ 2)) (Nullary x))
divVars = forVars Divide (\x -> Nullary (Integ 1))
powVars = forVars Power (\x -> Binary Power (Nullary x) (Nullary x))
logVars = forVars Logarithm (\x -> Binary Logarithm (Nullary x) (Nullary x))

addNums = forNums Add (+)
subNums = forNums Subtract (-)
multNums = forNums Multiply (*)
divNums = forNums Divide (/)
powNums = forNums Power (**)
logNums = forNums Logarithm (logBase)

-- useful for quickchecking that solutions found through search equal solutions found through straight evaluation

evaluate :: Expression -> Double
evaluate (Unary Parens expression)          = evaluate expression
evaluate (Nullary (Constant value))             = value
evaluate (Nullary (Integ value))                = fromInteger value
evaluate (Unary Absolute value)             = abs (evaluate value)
evaluate (Unary Negate expression)          = negate (evaluate expression)
evaluate (Binary Add leftExpr rightExpr)            = evaluate leftExpr + evaluate rightExpr
evaluate (Binary Subtract leftExpr rightExpr)       = evaluate leftExpr - evaluate rightExpr
evaluate (Binary Multiply leftExpr rightExpr)        = evaluate leftExpr * evaluate rightExpr
evaluate (Binary Divide leftExpr rightExpr)         = evaluate leftExpr / evaluate rightExpr
evaluate (Binary Logarithm base expression)  = logBase (evaluate base) (evaluate expression)
evaluate (Binary Power  base expo)            = (evaluate base) ** (evaluate expo)
evaluate (Nullary (Variable str)) = error "Cannot evaluate a variable"
