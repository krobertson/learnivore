module Expressions
(
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
solveExpression = processExpression solve

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
varSolved (Nullary (Variable x)) = singleVariable (Nullary (Variable x))
varSolved (Unary op (Nullary (Variable _))) = True
varSolved (Binary op (Nullary (Variable _)) (Nullary (Constant _))) = True
varSolved (Binary op (Nullary (Constant _)) (Nullary (Variable _))) = True
varSolved (Binary op (Nullary (Variable _)) (Nullary (Integ _)))    = True
varSolved (Binary op (Nullary (Integ _)) (Nullary (Variable _)))    = True
varSolved _ = False

constSolved :: Expression -> Bool
constSolved (Nullary (Constant _)) = True
constSolved (Nullary (Integ _)) = True
constSolved _ = False

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

-- helpers
isNum :: Term -> Bool
isNum (Integ _) = True
isNum (Constant _) = True
isNum x = False

isZero :: Term -> Bool
isZero (Integ 0) = True
isZero (Constant 0.0) = True
isZero _ = False

isOne :: Term -> Bool
isOne (Integ 0) = True
isOne (Constant 0.0) = True
isOne _ = False

isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

isVariableProduct :: Expression -> Bool
isVariableProduct (Binary Multiply (Nullary x) (Nullary y))
                  | isVariable x && isNum y = True
                  | isVariable y && isNum y = True
isVariableProduct _ = False

-- expressionTransformations
expressionTransformations = [absolutify, multiplyByZero, multiplyByOne, distribute, 
                            collapseAdd, collapseSub, pop, logify, exponentiate, negatify]

pop :: Expression -> Expression
pop (Unary Parens x) = x
pop x = x

multiplyByZero :: Expression -> Expression
multiplyByZero (Binary Multiply (Nullary x) y) 
               | isZero x = Nullary (Integ 0)
               | otherwise = (Binary Multiply (Nullary x) y)
multiplyByZero (Binary Multiply x (Nullary y)) 
               | isZero y = Nullary (Integ 0)
               | otherwise = (Binary Multiply x (Nullary y))
multiplyByZero x = x

multiplyByOne :: Expression -> Expression
multiplyByOne (Binary Multiply (Nullary x) y) 
               | isOne x = y
               | otherwise = (Binary Multiply (Nullary x) y)
multiplyByOne (Binary Multiply x (Nullary y)) 
               | isOne y = x
               | otherwise = (Binary Multiply x (Nullary y))
multiplyByOne x = x

absolutify :: Expression -> Expression
absolutify (Unary Absolute (Unary Negate x)) = x
absolutify (Unary Absolute x) = x
absolutify expression = expression

logify :: Expression -> Expression
logify (Binary Logarithm base (Binary Multiply x y)) = Binary Add (Binary Logarithm base x) (Binary Logarithm base y)
logify (Binary Logarithm (Nullary (Constant x)) (Nullary (Constant y))) = Nullary (Constant (logBase x y))
logify (Binary Logarithm (Nullary (Integ x)) (Nullary (Integ y))) = Nullary (Constant (logBase (fromInteger x) (fromInteger y)))
logify (Binary Logarithm (Nullary (Integ x)) (Nullary (Constant y))) = Nullary (Constant (logBase (fromInteger x) y))
logify (Binary Logarithm (Nullary (Constant x)) (Nullary (Integ y))) = Nullary (Constant (logBase x (fromInteger y)))
logify expression = expression

exponentiate :: Expression -> Expression
exponentiate (Binary Power (Nullary (Constant x)) ((Nullary (Constant y)))) = Nullary (Constant (x ** y))
exponentiate (Binary Power (Nullary (Integ x)) (Nullary (Integ y))) = Nullary (Constant ((fromInteger x) ** (fromInteger y)))
exponentiate (Binary Power (Nullary (Constant x)) (Nullary (Integ y))) = Nullary (Constant (x ** (fromInteger y)))
exponentiate (Binary Power (Nullary (Integ x)) (Nullary (Constant y))) = Nullary (Constant ((fromInteger x) ** y))
exponentiate expression = expression

negatify :: Expression -> Expression
negatify (Unary Negate (Nullary (Constant x))) = (Nullary (Constant (negate x)))
negatify (Unary Negate (Nullary (Integ x))) = (Nullary (Integ (negate x)))
negatify x = x

distribute :: Expression -> Expression
distribute (Binary Multiply (Binary Add x y) z) = Binary Add (Binary Multiply x z) (Binary Multiply y z)
distribute x = x

collapseAdd :: Expression -> Expression
collapseAdd (Binary Add (Nullary x) (Nullary y)) = addTerms x y
collapseAdd (Binary Add (Nullary (Variable x)) (Binary Multiply y z))
            | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                            ((Nullary (Variable var)):tl) -> Binary Multiply y (Binary Add z (Nullary (Integ 1)))
                                                            (hd:(Nullary (Variable var)):[]) -> Binary Multiply z (Binary Add y (Nullary (Integ 1)))
                                                            _ -> Binary Add (Nullary (Variable x)) (Binary Multiply y z)
            | otherwise = Binary Add (Nullary (Variable x)) (Binary Multiply y z)
collapseAdd (Binary Add (Binary Multiply y z) (Nullary (Variable x)))
            | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                            ((Nullary (Variable var)):tl) -> Binary Multiply y (Binary Add z (Nullary (Integ 1)))
                                                            (hd:(Nullary (Variable var)):[]) -> Binary Multiply z (Binary Add y (Nullary (Integ 1)))
                                                            _ -> Binary Add (Binary Multiply y z) (Nullary (Variable x))
            | otherwise = Binary Add (Binary Multiply y z) (Nullary (Variable x))
collapseAdd expression = expression

collapseSub :: Expression -> Expression
collapseSub (Binary Subtract (Nullary x) (Nullary y)) = subTerms x y
collapseSub (Binary Subtract (Nullary (Variable x)) (Binary Multiply y z))
            | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                          ((Nullary (Variable var)):tl) -> Binary Multiply y (Binary Subtract z (Nullary (Integ 1)))
                                                          (hd:(Nullary (Variable var)):[]) -> Binary Multiply z (Binary Subtract y (Nullary (Integ 1)))
                                                          _ -> Binary Subtract (Nullary (Variable x)) (Binary Multiply y z)
            | otherwise = Binary Add (Nullary (Variable x)) (Binary Multiply y z)
collapseSub (Binary Subtract (Binary Multiply y z) (Nullary (Variable x)))
            | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                            ((Nullary (Variable var)):tl) -> Binary Multiply y (Binary Subtract z (Nullary (Integ 1)))
                                                            (hd:(Nullary (Variable var)):[]) -> Binary Multiply z (Binary Subtract y (Nullary (Integ 1)))
                                                            _ -> Binary Subtract (Binary Multiply y z) (Nullary (Variable x))
            | otherwise = Binary Subtract (Binary Multiply y z) (Nullary (Variable x))
collapseSub expression = expression

collapseMult :: Expression -> Expression
collapseMult (Binary Multiply (Nullary x) (Nullary y)) = multTerms x y
collapseMult (Binary Multiply (Nullary (Variable x)) (Binary Multiply y z))
             | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                             ((Nullary (Variable var)):tl) -> Binary Multiply y (Binary Add z (Nullary (Integ 1)))
                                                             (hd:(Nullary (Variable var)):[]) -> Binary Multiply z (Binary Add y (Nullary (Integ 1)))
                                                             _ -> Binary Multiply (Nullary (Variable x)) (Binary Multiply y z)
             | otherwise = Binary Add (Nullary (Variable x)) (Binary Multiply y z)
collapseMult (Binary Multiply (Binary Multiply y z) (Nullary (Variable x)))
             | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                             ((Nullary (Variable var)):tl) -> Binary Multiply y (Binary Add z (Nullary (Integ 1)))
                                                             (hd:(Nullary (Variable var)):[]) -> Binary Multiply z (Binary Add y (Nullary (Integ 1)))
                                                             _ -> Binary Add (Binary Multiply y z) (Nullary (Variable x))
             | otherwise = Binary Add (Binary Multiply y z) (Nullary (Variable x))
collapseMult expression = expression

collapseDiv :: Expression -> Expression
collapseDiv (Binary Divide (Nullary x) (Nullary y)) = divTerms x y
collapseDiv (Binary Divide (Nullary (Variable x)) (Binary Divide y z))
            | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                          ((Nullary (Variable var)):tl) -> z
                                                          (hd:(Nullary (Variable var)):[]) -> Binary Divide (Nullary (Integ 1)) y
                                                          _ -> Binary Divide (Nullary (Variable x)) (Binary Multiply y z)
            | otherwise = Binary Add (Nullary (Variable x)) (Binary Multiply y z)
collapseDiv (Binary Divide (Binary Multiply y z) (Nullary (Variable x)))
            | ([x] == (nub $ listOfVariables y ++ listOfVariables z)) = case [y,z] of
                                                            ((Nullary (Variable var)):tl) -> z
                                                            (hd:(Nullary (Variable var)):[]) -> y
                                                            _ -> Binary Divide (Binary Multiply y z) (Nullary (Variable x))
            | otherwise = Binary Divide (Binary Multiply y z) (Nullary (Variable x))
collapseDiv expression = expression

-- collapse helpers
addTerms :: Term -> Term -> Expression
addTerms x y 
         | isNum x && isNum y = Nullary (addNums x y)
         | isVariable x && isVariable y && x == y = Binary Multiply (Nullary (Integ 2)) (Nullary x)
         | otherwise = Binary Add (Nullary x) (Nullary y)

addNums :: Term -> Term -> Term
addNums (Integ a) (Integ b) = Integ (a + b)
addNums (Constant x) (Integ y) = (Constant (x + (fromInteger y)))
addNums (Integ x) (Constant y) = (Constant ((fromInteger x) + y))

subTerms :: Term -> Term -> Expression
subTerms x y 
         | isNum x && isNum y = Nullary (subNums x y)
         | isVariable x && isVariable y && x == y = Nullary (Integ 0)
         | otherwise = Binary Subtract (Nullary x) (Nullary y)

subNums :: Term -> Term -> Term
subNums (Integ x) (Integ y) = (Integ (x - y))
subNums (Constant x) (Integ y) = (Constant (x - (fromInteger y)))
subNums (Integ x) (Constant y) = (Constant ((fromInteger x) - y))

multTerms :: Term -> Term -> Expression
multTerms x y
          | isNum x && isNum y = Nullary (multNums x y)
          | isVariable x && isVariable y && x == y = Binary Multiply (Nullary (Integ 2)) (Nullary x)
          | otherwise = Binary Multiply (Nullary x) (Nullary y)
          
multNums :: Term -> Term -> Term
multNums (Integ x) (Integ y) = (Integ (x * y))
multNums (Constant x) (Integ y) = (Constant (x * (fromInteger y)))
multNums (Integ x) (Constant y) = (Constant ((fromInteger x) * y))

divTerms :: Term -> Term -> Expression
divTerms x y
         | isNum x && isNum y = Nullary (divNums x y)
         | isVariable x && isVariable y && x == y = Nullary (Integ 1)
         | otherwise = Binary Divide (Nullary x) (Nullary y)
         
divNums :: Term -> Term -> Term
divNums (Integ x) (Integ y) = (Constant ((fromInteger x) / (fromInteger y)))
divNums (Constant x) (Integ y) = (Constant (x / (fromInteger y)))
divNums (Integ x) (Constant y) = (Constant ((fromInteger x) / y))

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