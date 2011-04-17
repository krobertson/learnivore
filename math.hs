import List
import Data.Tree
import Control.Monad(liftM)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.Combinator

join :: String -> [String] -> String
join _ [] = ""
join str xs = foldl1 (\x y -> x ++ str ++ y) xs

around :: String -> String -> String -> String
around obj start end = start ++ obj ++ end

parenthesize x = around x "(" ")"
angleBracket x = around x "<" ">"

printBetween :: (Show a) => String -> [a] -> IO ()
printBetween str xs = print $ join str (map show xs)

--- Exprs, a data type for containing simple binary expressions (for easy parsing)

data Expr = Var String | Const Double | Uno Unop (Expr) | Duo Duop (Expr) (Expr) | Seq [Expr]
    deriving Show
data Unop = Neg | Abs deriving Show
data Duop = Mult | Div | Add | Sub | Log | Pow deriving Show


exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "-" >> return (Uno Neg))]
        , [Infix (m_reservedOp "^" >> return (Duo Pow)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (Duo Mult)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (Duo Div)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
        ]
        
term = m_parens exprparser
       <|> liftM Var m_identifier
       <|> liftM Const m_float
       <|> do {
                char '|'
              ; x <- exprparser
              ; char '|'
              ; return (Uno Abs x)
              }
       <|> do {
                string "log"
              ; char '<'
              ; x <- exprparser
              ; char '>'
              ; char '('
              ; y <- m_parens exprparser
              ; char ')'
              ; return (Duo Log x y)
              }

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = letter
              , opStart = oneOf "|*-+=:^/"
              , opLetter = oneOf "|*-+=:^/"
              , reservedOpNames = ["*", "+", "-", "^", "/", "=", ":="]
              , reservedNames = ["log"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , float = m_float
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

-- loadExpr :: String -> Either ParseError Expr
-- loadExpr inp = case parse exprparser "" inp of
--                { Left err -> return err
--                ; Right ans -> return ans
--                }
--                
-- printExpr :: Either ParseError Expr -> IO ()
-- printExpr (Left err) = print err
-- printExpr (Right expr) = print expr

-- getExpr :: Either ParseError Expr -> Expr
-- getExpr (Left _) = Const 0.0
-- getExpr (Right x) = x
-- 

--- Expressions

data Expression = Constant Double
                | Absolute (Expression)
                | Negate (Expression)
                | Sum [Expression]
                | Subtract [Expression]
                | Product [Expression]
                | Divide [Expression]
                | Power (Expression) (Expression)
                | Logarithm (Expression) (Expression)
                  deriving (Eq, Ord)

instance Show Expression where
  show = showExpression

emap :: (Expression -> Expression) -> Expression -> Expression
emap fn (Constant x) = (Constant x)
emap fn (Absolute x) = Absolute (emap fn x)
emap fn (Negate x) = Negate (emap fn x)
emap fn (Sum xs) = Sum (map (emap fn) xs)
emap fn (Subtract xs) = Subtract (map (emap fn) xs)
emap fn (Product xs) = Product (map (emap fn) xs)
emap fn (Divide xs) = Divide (map (emap fn) xs)
emap fn (Power x y) = Power (emap fn x) (emap fn y)
emap fn (Logarithm x y) = Logarithm (emap fn x) (emap fn y)
-- displaying an expression to the end user

showExpression :: Expression -> String
showExpression (Product xs) = join " * " (map showExpression' xs)
showExpression (Divide xs) = join " / " (map showExpression' xs)
showExpression (Sum xs) = join " + " (map showExpression' xs)
showExpression (Subtract xs) = join " - " (map showExpression' xs)
showExpression expression = showExpression' expression

showExpression' :: Expression -> String
showExpression' (Constant a) = show a
showExpression' (Negate a) = "-" ++ showExpression' a
showExpression' (Absolute a) = around (showExpression a) "|" "|"
showExpression' (Power a b) = showExpression' a ++ "^" ++ showExpression' b
showExpression' (Logarithm base a) = "log" ++ angleBracket (showExpression base) ++ parenthesize (showExpression a)
showExpression' expression = parenthesize $ showExpression expression

-- Parsing Expressions via Expr

exprToExpression :: Expr -> Expression
exprToExpression (Const x) = Constant x
exprToExpression (Uno Neg x) = Negate (exprToExpression x)
exprToExpression (Uno Abs x) = Absolute (exprToExpression x)
exprToExpression (Duo Pow x y) = Power (exprToExpression x) (exprToExpression y)
exprToExpression (Duo Mult x y) = Product [(exprToExpression x), (exprToExpression y)]
exprToExpression (Duo Div x y) = Divide [(exprToExpression x), (exprToExpression y)]
exprToExpression (Duo Add x y) = Sum [(exprToExpression x), (exprToExpression y)]
exprToExpression (Duo Sub x y) = Subtract [(exprToExpression x), (exprToExpression y)]
exprToExpression (Duo Log b x) = Logarithm (exprToExpression b) (exprToExpression x)

printExpression inp = case parse exprparser "" inp of
             { Left err -> return (Constant 0)
             ; Right ans -> return (exprToExpression $ ans)
             }
  
data Solution = Solution [Expression]

showSolution :: Solution -> String
showSolution (Solution xs) = join "\n=>\n" (map show xs)

instance Show Solution where
  show = showSolution

bft :: Tree a -> [a]
bft t = map rootLabel $
      concat $
      takeWhile (not . null) $               
      iterate (concatMap subForest) [t]

takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough fn xs = (takeWhile fn xs) ++ [(head (dropWhile fn xs))]

solve :: Expression -> Solution
solve expression = Solution solutionPath
                    where solutionPath = takeThrough (not . solved) $ 
                                         bft . solutionTree $ expression

collapse :: Expression -> Expression
collapse expression = head $ dropWhile (not . solved) $ 
                      bft . solutionTree $ expression 
                 
solved :: Expression -> Bool
solved (Constant x) = True
solved (Negate (Constant x)) = True
solved x = False
  
printSolution :: Solution -> IO ()
printSolution = putStrLn . showSolution

solutionTree :: Expression -> Tree Expression
solutionTree expression = Node expression (map (solutionTree . rootLabel) $ expand expression)

expand :: Expression -> Forest Expression
expand = twiddle transformations

twiddle :: [Expression -> Expression] -> Expression -> Forest Expression
twiddle transforms expression = if (not . solved $ expression) 
                                then filter (\x -> not (rootLabel x == expression)) $
                                  map (\x -> Node (x expression) []) transforms
                                else []
                                    

transformations = [squash, absolutify, logify, 
                   multiplyByZero, multiplyByOne, distribute, 
                   collapseSum, collapseProduct, (emap sortExpression)]

-- transformations
sortExpression :: Expression -> Expression
sortExpression (Sum xs) = Sum (sort (map sortExpression xs))
sortExpression (Subtract xs) = Subtract (map sortExpression xs)
sortExpression (Product xs) = Product (sort (map sortExpression xs))
sortExpression (Divide xs) = Divide (map sortExpression xs)
sortExpression (Power x y) = Power (sortExpression x) (sortExpression y)
sortExpression (Logarithm x y) = Logarithm (sortExpression x) (sortExpression y)
sortExpression (Absolute x) = Absolute (sortExpression x)
sortExpression (Negate y) = Negate (sortExpression y)
sortExpression z = z

multiplyByZero :: Expression -> Expression
multiplyByZero (Product xs) = if any (\x -> x == Constant 0.0) xs then Constant 0.0 else (Product (map multiplyByZero xs))
multiplyByZero xs = xs

multiplyByOne :: Expression -> Expression
multiplyByOne (Product xs) = Product (filter (\x -> not (x == Constant 1.0)) (map multiplyByOne xs))
multiplyByOne xs = xs

absolutify :: Expression -> Expression
absolutify (Absolute (Negate x)) = x
absolutify (Absolute x) = x
absolutify expression = expression

logify :: Expression -> Expression
logify (Logarithm (Constant x) (Constant y)) = Constant (logBase x y)
logify expression = expression

distribute :: Expression -> Expression
distribute (Product (x:[])) = x
distribute (Product ((Sum xs):ys)) = Sum (map (\x -> Product (x:ys)) xs)
distribute (Product (x:(Sum ys):zs)) = Sum (map (\y -> Product (x:(y:zs))) ys)
distribute x = x

squash :: Expression -> Expression
squash (Sum xs) = squashSum (Sum (map squash xs))
squash (Subtract xs) = squashSubtract (Subtract (map squash xs))
squash (Product xs) = squashProduct (Product (map squash xs))
squash (Divide xs) = squashDivide (Divide (map squash xs))
squash (Logarithm x y) = Logarithm (squash x) (squash y)
squash (Power x y) = Power (squash x) (squash y)
squash (Absolute x) = Absolute (squash x)
squash (Negate x) = Negate (squash x)
squash x = x


squashSum (Sum (x:[])) = x
squashSum (Sum ((Sum xs):ys)) = Sum (xs ++ ys)
squashSum (Sum (x:(Sum xs):ys)) = Sum (x:xs ++ ys)
squashSum x = x

squashProduct (Product (x:[])) = x
squashProduct (Product ((Product xs):ys)) = Product (xs ++ ys)
squashProduct (Product (x:(Product xs):ys)) = Product (x:xs ++ ys)
squashProduct x = x

squashSubtract = undefined
squashDivide = undefined

-- multiplicationTransformations = [multiplyOne, multiplyZero, distributeMult, concentratePower]
-- divideTransformations = [toMult]
-- sumTransformations = [addZero, concentrateMult, concentrateLog]
-- subtractTransformations = []
-- logTransformations = [splitMult]
-- powerTransformations = []
-- absoluteTransformations = []
-- negativeTransformations = [doubleInversion]

-- merging
fix :: (a -> a) -> a
fix f = f (fix f)

add :: Expression -> Expression -> Expression
add (Constant a) (Constant b) = Constant (a + b)
add (Sum xs) expression = Sum (expression:xs)
add expression (Sum xs) = Sum (expression:xs)
add (Constant a) expression = Sum ((Constant a):[expression])
add expression (Constant a) = Sum ((Constant a):[expression])
add expression1 expression2 = Sum [expression1, expression2]

myNegate :: Expression -> Expression
myNegate (Constant num) = Constant (-(num))
myNegate (Negate expression) = expression
myNegate expression = Negate expression

multiply :: Expression -> Expression -> Expression
multiply (Constant a) (Constant b) = Constant (a * b)
multiply (Constant a) expression = Product [Constant a, expression]
multiply (Product xs) ys = Product (xs ++ [ys])
multiply xs (Product ys) = Product (xs:ys)
multiply expression (Constant a) = multiply (Constant a) expression
multiply (Negate a) (Negate b) = multiply a b
multiply (Negate a) expression = Product [Negate a, expression]
multiply expression1 expression2 = Product [expression1, expression2] 

-- collapsing --

collapseSum :: Expression -> Expression
collapseSum (Sum xs) = foldl add (Constant 0) xs
collapseSum xs = emap collapseSum xs

collapseProduct :: Expression -> Expression
collapseProduct (Product xs) = foldl multiply (Constant 1.0) xs
collapseProduct xs = emap collapseProduct xs

evaluate :: Expression -> Double
evaluate (Constant value)             = value
evaluate (Absolute value)             = abs (evaluate value)
evaluate (Negate expression)          = -(evaluate expression)
evaluate (Sum expressions)            = foldr ((+) . evaluate) 0 expressions
evaluate (Product expressions)        = foldr ((*) . evaluate) 1 expressions
evaluate (Logarithm base expression)  = logBase (evaluate base) (evaluate expression)


-- equations

data Equation = Equation {
  lhs :: Expression,
  rhs :: Expression
}

instance (Show (Equation)) where
  show (Equation expr1 expr2) = (show expr1) ++ " = " ++ (show expr2)

valid :: Equation -> Bool
valid (Equation lhs rhs) = (evaluate lhs) == (evaluate rhs)