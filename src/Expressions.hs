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
                                
exmap :: (Expression -> Expression) -> Expression -> Expression
exmap fn (Parens x) = fn x
exmap fn (Sum xs) = fn $ Sum (List.map (exmap fn) xs)
exmap fn (Subtract xs) = fn $ Subtract (List.map (exmap fn) xs)
exmap fn (Product xs) = fn $ Product (List.map (exmap fn) xs)
exmap fn (Divide xs) = fn $ Divide (List.map (exmap fn) xs)
exmap fn (Power x y) = fn $ Power (exmap fn x) (exmap fn y)
exmap fn (Logarithm x y) = fn $ Logarithm (exmap fn x) (exmap fn y)
exmap fn (Absolute x) = fn $ Absolute (exmap fn x)
exmap fn (Negate y) = fn $ Negate (exmap fn y)
exmap fn z = fn z

expressionSize :: Expression -> Integer
expressionSize (Parens x) = expressionSize x
expressionSize (Negate x) = expressionSize x
expressionSize (Absolute x) = 1 + expressionSize x
expressionSize (Sum xs) = foldl (+) 1 $ List.map expressionSize xs
expressionSize (Subtract xs) = foldl (+) 1 $ List.map expressionSize xs
expressionSize (Product xs) = foldl (+) 1 $ List.map expressionSize xs
expressionSize (Divide xs) = foldl (+) 1 $ List.map expressionSize xs
expressionSize (Power x y) = 1 + expressionSize x + expressionSize y
expressionSize (Logarithm x y) = 1 + expressionSize x + expressionSize y
expressionSize _ = 1

-- solution checkers
solved :: Expression -> Bool
solved (Absolute x) = varSolved x
solved x = (constSolved x) || (varSolved x)

varSolved :: Expression -> Bool
varSolved (Variable x) = singleVariable (Variable x)
varSolved (Negate (Variable _)) = True
varSolved (Product [(Variable _), (Constant _)]) = True
varSolved (Product [(Constant _), (Variable _)]) = True
varSolved (Product [(Variable _), (Integ _)])    = True
varSolved (Product [(Integ _), (Variable _)])    = True
varSolved (Divide [(Variable _), (Constant _)])  = True
varSolved (Divide [(Constant _), (Variable _)])  = True
varSolved (Divide [(Variable _), (Integ _)])     = True
varSolved (Divide [(Integ _), (Variable _)])     = True
varSolved (Sum [(Variable _), (Constant _)])     = True
varSolved (Sum [(Constant _), (Variable _)])     = True
varSolved (Sum [(Variable _), (Integ _)])        = True
varSolved (Sum [(Integ _), (Variable _)])        = True
varSolved (Logarithm (Variable str) x)
          | constSolved x = True
          | (numberOfVariables (Logarithm (Variable str) x)) == 1 = True
          | otherwise = False
varSolved (Logarithm x (Variable str))
          | constSolved x = True
          | (numberOfVariables (Logarithm x (Variable str))) == 1 = True
          | otherwise = False
varSolved (Power (Variable str) x)
          | constSolved x = True
          | (numberOfVariables (Power (Variable str) x)) == 1 = True
          | otherwise = False
varSolved (Power x (Variable str))
          | constSolved x = True
          | (numberOfVariables (Power x (Variable str))) == 1 = True
          | otherwise = False
varSolved _ = False

constSolved :: Expression -> Bool
constSolved (Constant _) = True
constSolved (Integ _) = True
constSolved _ = False

singleVariable :: Expression -> Bool
singleVariable (Variable _) = True
singleVariable _ = False

numberOfVariables :: Expression -> Int
numberOfVariables expression = length . listOfVariables $ expression

listOfVariables :: Expression -> [String]
listOfVariables (Variable str) = [str]
listOfVariables (Absolute x) = listOfVariables x
listOfVariables (Negate x) = listOfVariables x
listOfVariables (Product xs) = nub . concatMap listOfVariables $ xs
listOfVariables (Divide xs) = nub . concatMap listOfVariables $ xs
listOfVariables (Sum xs) = nub . concatMap listOfVariables $ xs
listOfVariables (Subtract xs) = nub . concatMap listOfVariables $ xs
listOfVariables (Logarithm x y) = nub . concatMap listOfVariables $ x:y:[]
listOfVariables (Power x y) = nub . concatMap listOfVariables $ x:y:[]
listOfVariables _ = []

-- expressionTransformations

sortExpression :: Expression -> Expression
sortExpression (Sum xs) = Sum (sort xs)
sortExpression (Subtract (x:xs)) = Subtract (x:(sort xs))
sortExpression (Product xs) = Product (sort xs)
sortExpression (Divide (x:xs)) = Divide (x:(sort xs))
sortExpression (Power x y) = Power x y
sortExpression (Logarithm x y) = Logarithm x y
sortExpression (Absolute x) = Absolute x
sortExpression (Negate y) = Negate y
sortExpression z = z

multiplyByZero :: Expression -> Expression
multiplyByZero (Product xs) = if (any (\x -> (x == Constant 0.0) || (x == Integ 0.0)) (List.map multiplyByZero xs)) then Integ 0.0 else (Product xs)
multiplyByZero xs = xs

multiplyByOne :: Expression -> Expression
multiplyByOne (Product xs) = Product (List.filter (\x -> not $ (x == Constant 1.0) || (x == Integ 1.0)) (List.map multiplyByOne xs))
multiplyByOne xs = xs

absolutify :: Expression -> Expression
absolutify (Absolute (Negate x)) = x
absolutify (Absolute x) = x
absolutify expression = expression

logify :: Expression -> Expression
logify (Logarithm x (Product xs)) = Sum (List.map (Logarithm x) xs)
logify (Logarithm (Constant x) (Constant y)) = Constant (logBase x y)
logify (Logarithm (Integ x) (Integ y)) = Constant (logBase x y)
logify (Logarithm (Integ x) (Constant y)) = Constant (logBase x y)
logify (Logarithm (Constant x) (Integ y)) = Constant (logBase x y)
logify expression = expression

exponentiate :: Expression -> Expression
exponentiate (Power (Constant x) (Constant y)) = Constant (x ** y)
exponentiate (Power (Integ x) (Integ y)) = Constant (x ** y)
exponentiate (Power (Constant x) (Integ y)) = Constant (x ** y)
exponentiate (Power (Integ x) (Constant y)) = Constant (x ** y)
exponentiate expression = expression

negatify :: Expression -> Expression
negatify (Negate (Constant x)) = (Constant (0-x))
negatify (Negate (Integ x)) = (Integ (0-x))
negatify x = x

distribute :: Expression -> Expression
distribute (Product (x:[])) = x
distribute (Product ((Sum xs):ys)) = Sum (List.map (\x -> Product (x:ys)) xs)
distribute (Product (x:(Sum ys):zs)) = Sum (List.map (\y -> Product (x:(y:zs))) ys)
distribute x = x

squash :: Expression -> Expression
squash (Sum xs) = squashSum (Sum (List.map squash xs))
squash (Subtract xs) = squashSubtract (Subtract (List.map squash xs))
squash (Product xs) = squashProduct (Product (List.map squash xs))
squash (Divide xs) = squashDivide (Divide (List.map squash xs))
squash (Logarithm x y) = Logarithm (squash x) (squash y)
squash (Power x y) = Power (squash x) (squash y)
squash (Absolute x) = Absolute (squash x)
squash (Negate x) = Negate (squash x)
squash (Parens x) = squash x
squash x = x


squashSum (Sum (x:[])) = x
squashSum (Sum ((Sum xs):ys)) = Sum (xs ++ ys)
squashSum (Sum (x:(Sum xs):ys)) = Sum (x:xs ++ ys)
squashSum x = x

squashProduct (Product (x:[])) = x
squashProduct (Product ((Product xs):ys)) = Product (xs ++ ys)
squashProduct (Product (x:(Product xs):ys)) = Product (x:xs ++ ys)
squashProduct x = x

squashSubtract x = x
squashDivide x = x

collapseSum :: Expression -> Expression
collapseSum (Sum xs) = foldl add (Constant 0) xs
collapseSum xs = xs

collapseProduct :: Expression -> Expression
collapseProduct (Product xs) = foldl multiply (Constant 1.0) xs
collapseProduct xs = xs

collapseDivide :: Expression -> Expression
collapseDivide (Divide (x:xs)) = foldl divide x xs
collapseDivide xs = xs

collapseSubtract :: Expression -> Expression
collapseSubtract (Subtract (x:xs)) = foldl sub x xs
collapseSubtract xs = xs

expressionTransformations = [absolutify, multiplyByZero, multiplyByOne, distribute, 
                   collapseSum, collapseSubtract, collapseProduct, collapseDivide, squash, --collapseVariables,
                   logify, exponentiate, negatify, sortExpression]
-- merging

add :: Expression -> Expression -> Expression
add (Constant a) (Constant b) = Constant (a + b)
add (Variable x) (Variable y) 
                            | x == y = Product [Integ 2.0, Variable x]
                            | otherwise = Sum [Variable x, Variable y]
add (Integ a) (Integ b) = Integ (a + b)
add (Constant a) (Integ b) = Constant (a + b)
add (Integ a) (Constant b) = Constant (a + b)
add (Sum xs) expression = Sum (expression:xs)
add expression (Sum xs) = Sum (expression:xs)
add (Constant a) expression = Sum ((Constant a):[expression])
add expression (Constant a) = Sum ((Constant a):[expression])
add (Integ a) expression = Sum ((Integ a):[expression])
add expression (Integ a) = Sum ((Integ a):[expression])
add expression1 expression2 = Sum [expression1, expression2]

sub :: Expression -> Expression -> Expression
sub (Variable x) (Variable y) 
                            | x == y = Integ 0.0
                            | otherwise = Subtract [Variable x, Variable y]
sub (Constant a) (Constant b) = Constant (a - b)
sub (Integ a) (Integ b) = Integ (a - b)
sub (Constant a) (Integ b) = Constant (a - b)
sub (Integ a) (Constant b) = Constant (a - b)
sub (Subtract xs) expression = Subtract (expression:xs)
sub expression (Sum xs) = Subtract (expression:xs)
sub (Constant a) expression = Subtract ((Constant a):[expression])
sub expression (Constant a) = Subtract ((Constant a):[expression])
sub (Integ a) expression = Subtract ((Integ a):[expression])
sub expression (Integ a) = Subtract ((Integ a):[expression])
sub expression1 expression2 = Subtract [expression1, expression2]

myNegate :: Expression -> Expression
myNegate (Constant num) = Constant (-(num))
myNegate (Integ num) = Integ (-(num))
myNegate (Negate expression) = expression
myNegate expression = Negate expression

multiply :: Expression -> Expression -> Expression
multiply (Variable x) (Variable y)
                      | x == y = (Power (Variable x) (Integ 2.0))
                      | otherwise = Product [Variable x, Variable y]
multiply (Constant a) (Constant b) = Constant (a * b)
multiply (Integ a) (Constant b) = Constant (a * b)
multiply (Constant a) (Integ b) = Constant (a * b)
multiply (Integ a) (Integ b) = Integ (a * b)
multiply (Constant a) expression = Product [Constant a, expression]
multiply (Integ a) expression = Product [Integ a, expression]
multiply (Product xs) ys = Product (xs ++ [ys])
multiply xs (Product ys) = Product (xs:ys)
multiply expression (Constant a) = multiply (Constant a) expression
multiply expression (Integ a) = multiply (Integ a) expression
multiply (Negate a) (Negate b) = multiply a b
multiply (Negate a) expression = Product [Negate a, expression]
multiply expression1 expression2 = Product [expression1, expression2]

divide :: Expression -> Expression -> Expression
divide (Variable x) (Variable y)
                    | x == y = Integ 1.0
                    | otherwise = Divide [Variable x, Variable y]
divide (Constant a) (Constant b) = Constant (a / b)
divide (Integ a) (Constant b) = Constant (a / b)
divide (Constant a) (Integ b) = Constant (a / b)
divide (Integ a) (Integ b) = Constant (a / b)
divide (Constant a) expression = Divide [Constant a, expression]
divide (Integ a) expression = Divide [Integ a, expression]
divide (Divide xs) y = Divide (xs ++ [y])
divide x (Divide (y:ys)) = Divide ((Product ((x:ys)):[y]))
divide (Negate a) (Negate b) = divide a b
divide expression1 expression2 = Divide [expression1, expression2]

-- useful for quickchecking that solutions found through search equal solutions found through straight evaluation

evaluate :: Expression -> Double
evaluate (Constant value)             = value
evaluate (Integ value)                = value
evaluate (Absolute value)             = abs (evaluate value)
evaluate (Negate expression)          = -(evaluate expression)
evaluate (Sum expressions)            = foldr ((+) . evaluate) 0 expressions
evaluate (Subtract expressions)       = foldl (\x y -> x - (evaluate y)) (evaluate . head $ expressions) (tail expressions)
evaluate (Product expressions)        = foldr ((*) . evaluate) 1 expressions
evaluate (Divide expressions)         = foldl (\x y -> x / (evaluate y)) (evaluate . head $ expressions) (tail expressions)
evaluate (Logarithm base expression)  = logBase (evaluate base) (evaluate expression)
evaluate (Power base expo)            = (evaluate base) ** (evaluate expo)
evaluate (Variable str) = error "Cannot evaluate a variable"