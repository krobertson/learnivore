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
varSolved (Unary Parens _) = False
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
expressionTransformations = [
                             ("Removing Parentheses", unparen), ("Negation", negation),
                             ("Adding", addition), ("Subtracting", subtraction), 
                             ("Multiplying", multiplication), ("Dividing", division), 
                             ("Taking the Logarithm", takethelog), 
														 ("Exponentiating with Negative Powers", negexp), ("Exponentiating", exponentiation),
                             ("Taking the Nth Root", root), ("Taking the Absolute Value", absval), 
                             ("Adding Zero", addzero), ("Multiplying By Zero", multzero), 
                             ("Dividing a Zero", divzero), ("Multiplying two Negatives", multnegs),
                             ("Multiplying By One", multone), ("Distribute Law of Multiplication", dist),
                             ("Inverse Law of Logarithms", loginverse), ("Inverse Law of Powers", powinverse), 
                             ("Collapsing Variables", collvar),
                             ("Swapping Commutative Order", commute), ("Swapping Associative Evaluation Order", associate)
                            ]

transform :: (Expression -> Expression) -> (Expression -> [Expression])
transform fn = propagate ((:[]) . fn)

root = transform applyRoot
unparen = transform pop
addzero = transform addZero
divzero = transform divideZero
multzero = transform multiplyByZero
negation = transform negatify
addition = transform applyAdd
subtraction = transform applySub
multiplication = transform applyMult
exponentiation = transform applyPow
division = transform applyDiv
takethelog = transform applyLog
negexp = transform negativePowers
absval = transform absolutify
multnegs = transform multiplyNegatives
multone = transform multiplyByOne
dist = transform distribute
loginverse = transform logInverse
powinverse = transform powInverse
collvar = transform collapseVars



applyRoot :: Expression -> Expression
applyRoot (Binary NthRoot n expr)
          | isNumExpr n && isNumExpr expr = val $ (value expr) `nthRoot` (value n)
          | otherwise = expr `nroot` n
applyRoot x = x

pop :: Expression -> Expression
pop (Unary Parens x) = x
pop x = x

addZero :: Expression -> Expression
addZero (Binary Add x y) 
        | isZeroExpr x = y
        | isZeroExpr y = x
        | otherwise = x |+| y
addZero x = x

divideZero :: Expression -> Expression
divideZero (Binary Divide x y)
           | isZeroExpr x = val 0
           | otherwise = x |/| y
divideZero x = x

multiplyByZero :: Expression -> Expression
multiplyByZero (Binary Multiply x y) 
               | isZeroExpr x || isZeroExpr y = val 0
               | otherwise = x |*| y
multiplyByZero x = x

multiplyByOne :: Expression -> Expression
multiplyByOne (Binary Multiply x y) 
               | eitherOr isOneExpr x y && isOneExpr x = y
               | eitherOr isOneExpr x y && isOneExpr y = x
               | otherwise = x |*| y
multiplyByOne x = x

multiplyNegatives :: Expression -> Expression
multiplyNegatives (Binary Multiply (Unary Negate x) (Unary Negate y)) = (Binary Multiply x y)
multiplyNegatives x = x 

absolutify :: Expression -> Expression
absolutify (Unary Absolute (Unary Negate x)) = x
absolutify expr@(Unary Absolute subex@(Nullary x))
           | (isNum x) && (value subex) < 0 = val $ abs (value subex)
           | otherwise = subex
absolutify expr@(Unary Absolute x)
           | isTerm x = x
           | otherwise = expr
absolutify expression = expression

negatify :: Expression -> Expression
negatify (Unary Negate (Unary Negate x)) = x
negatify x = x

negativePowers :: Expression -> Expression
negativePowers (Binary Power base (Unary Negate exponent)) = val 1 |/| base |^| exponent
negativePowers x = x

distribute :: Expression -> Expression
distribute (Binary Multiply (Binary Add x y) z) = x |*| z |+| y |*| z
distribute (Binary Multiply z (Binary Add x y)) = x |*| z |+| y |*| z
distribute x = x

logInverse :: Expression -> Expression
logInverse expr@(Binary Logarithm x (Binary Power x1 y)) = if x == x1 then y else expr
logInverse x = x

powInverse :: Expression -> Expression
powInverse expr@(Binary Power x (Binary Logarithm x1 y)) = if x == x1 then y else expr
powInverse x = x

collapseVars :: Expression -> Expression
collapseVars (Binary Add x y)
             | variable x == variable y && variable x /= "" = val 2 |*| x
             | (isVariableProduct x || variable x /= "") && 
               (isVariableProduct y || variable y /= "") && 
               (listOfVariables x List.\\ listOfVariables y == []) = (applyAdd ((getConstant x) |+| (getConstant y))) |*| (var (head $ listOfVariables x))
collapseVars x = x

-- functions for applying operators over terms
applyMult = applyBinOp Multiply multTerms
applySub  = applyBinOp Subtract subTerms
applyAdd  = applyBinOp Add addTerms
applyDiv  = applyBinOp Divide divTerms
applyLog  = applyBinOp Logarithm logTerms
applyPow  = applyBinOp Power powTerms

-- apply helpers
forNums :: BinaryOp -> (Double -> Double -> Double) -> Expression -> Expression -> Expression
forNums op fn x@(Nullary _) y@(Nullary _) = val (fn (value x) (value y))
forNums op fn x y
        | isTerm x && isTerm y = val (fn (value x) (value y))
        | otherwise = (bopConstructor op) x y

forVars :: BinaryOp -> (Expression -> Expression) -> Expression -> Expression -> Expression
forVars op fn v1@(Nullary (Variable x)) v2@(Nullary (Variable y))
        | x == y = fn v1
        | otherwise = (bopConstructor op) v1 v2
forVars op _ x y = (bopConstructor op) x y

forTerms numFn varFn x y
         | isNumExpr x && isNumExpr y = numFn x y
         | otherwise = varFn x y

applyBinOp op fn (Binary op2 t1 t2) 
      | op == op2 && isOK t1 && isOK t2 = fn t1 t2
      | isOK t1 && isOK t2 = (bopConstructor op2) t1 t2 
        where isOK x = isTerm x || isNumExpr x 
applyBinOp _ _ x = x

addTerms = forTerms addNums addVars
subTerms = forTerms subNums subVars
multTerms = forTerms multNums multVars
divTerms = forTerms divNums divVars
powTerms = forTerms powNums powVars
logTerms = forTerms logNums logVars

addVars = forVars Add (\x -> val 2 |*| x)
subVars = forVars Subtract (\x -> val 0)
multVars = forVars Multiply (\x -> x |^| val 2)
divVars = forVars Divide (\x -> val 1)
powVars = forVars Power (\x -> (x |^| x))
logVars = forVars Logarithm (\x -> val 1)

addNums = forNums Add (+)
subNums = forNums Subtract (-)
multNums = forNums Multiply (*)
divNums = forNums Divide (/)
powNums = forNums Power (**)
logNums = forNums Logarithm (logBase)

-- helpers
nthRoot x n = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)

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

numberOfVariables :: Expression -> Int
numberOfVariables expression = length . listOfVariables $ expression

listOfVariables :: Expression -> [String]
listOfVariables (Nullary (Variable str)) = [str]
listOfVariables (Unary op x) = listOfVariables x
listOfVariables (Binary op x y) = nub . concatMap listOfVariables $ [x,y]
listOfVariables _ = []

variable :: Expression -> String
variable (Nullary (Variable str)) = str
variable _ = ""

value :: Expression -> Double
value (Nullary (Constant n)) = n
value (Nullary (Integ n)) = fromInteger n
value (Nullary E) = exp 1
value (Nullary Pi) = pi
value (Unary Negate (Nullary (Constant n))) = -1 * n
value (Unary Negate (Nullary (Integ n))) = -1 * (fromInteger n)
value _ = 0.0 / 0.0


-- useful for quickchecking that solutions found through search equal solutions found through straight evaluation

evaluate :: Expression -> Double
evaluate (Unary Parens expression)          = evaluate expression
evaluate (Nullary (Variable str)) = error "Cannot evaluate a variable"
evaluate ex@(Nullary x)                        = value ex
evaluate (Unary Absolute value)             = abs (evaluate value)
evaluate (Unary Negate expression)          = negate (evaluate expression)
evaluate (Binary Add leftExpr rightExpr)            = evaluate leftExpr + evaluate rightExpr
evaluate (Binary Subtract leftExpr rightExpr)       = evaluate leftExpr - evaluate rightExpr
evaluate (Binary Multiply leftExpr rightExpr)        = evaluate leftExpr * evaluate rightExpr
evaluate (Binary Divide leftExpr rightExpr)         = evaluate leftExpr / evaluate rightExpr
evaluate (Binary Logarithm base expression)  = logBase (evaluate base) (evaluate expression)
evaluate (Binary Power  base expo)            = (evaluate base) ** (evaluate expo)