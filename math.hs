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
join _ (x:[]) = x
join str (x:xs) = x ++ str ++ (join str xs)

--- Exprs, a data type for containing simple binary expressions

data Expr = Var String | Const Double | Uno Unop (Expr) | Duo Duop (Expr) (Expr) | Seq [Expr]
    deriving Show
data Unop = Neg | Abs deriving Show
data Duop = Mult | Add | Log deriving Show

-- Parser for Exprs

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = letter
              , opStart = oneOf "|*-+=:"
              , opLetter = oneOf "|*-+=:"
              , reservedOpNames = ["*", "+", "-", "=", ":="]
              , reservedNames = ["log"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , float = m_float
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "-" >> return (Uno Neg))]
        , [Infix (m_reservedOp "*" >> return (Duo Mult)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
        ]
term = m_parens exprparser
       <|> liftM Var m_identifier
       <|> liftM Const m_float
       <|> do {
                string "log"
              ; char '<'
              ; x <- exprparser
              ; char '>'
              ; char '('
              ; y <- exprparser
              ; char ')'
              ; return (Duo Log x y)
              }
                     
-- use <|> to add more


printExpr :: String -> IO ()
printExpr inp = case parse exprparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

--- Expressions

data Expression = Sum [Expression]
                | Product [Expression]
                | Logarithm (Expression) (Expression)
                | Absolute (Expression)
                | Negate (Expression)
                | Constant Double
                  deriving (Eq)

-- Parsing Expressions via Expr

exprToExpression :: Expr -> Expression
exprToExpression (Const x) = Constant x
exprToExpression (Uno Neg x) = Negate (exprToExpression x)
exprToExpression (Uno Abs x) = Absolute (exprToExpression x)
exprToExpression (Duo Mult x y) = Product [(exprToExpression x), (exprToExpression y)]
exprToExpression (Duo Add x y) = Sum [(exprToExpression x), (exprToExpression y)]
exprToExpression (Duo Log b x) = Logarithm (exprToExpression b) (exprToExpression x)

loadExpression inp = case parse exprparser "" inp of
             { Left err -> return (Constant 0)
             ; Right ans -> return (simplify . exprToExpression $ ans)
             }

-- displaying an expression to the end user

showExpression :: Expression -> String
showExpression (Constant a) = show a
showExpression (Negate a) = "-" ++ showExpression a
showExpression (Absolute a) = "|" ++ showExpression a ++ "|"
showExpression (Logarithm base a) = "log<" ++ showExpression base ++ ">(" ++ showExpression a ++ ")"
showExpression (Product []) = ""
showExpression (Product (expression:[])) = showExpression expression
showExpression (Product expressions) = "(" ++ showProductExpressions expressions ++ ")"
showExpression (Sum []) = ""
showExpression (Sum (expression:[])) = showExpression expression
showExpression (Sum (expressions)) = "(" ++ showSumExpressions expressions ++ ")"

showProductExpressions :: [Expression] -> String
showProductExpressions xs = join " * " (map (\x -> showExpression x) xs)

showSumExpressions :: [Expression] -> String
showSumExpressions xs = join " + " (map (\x -> showExpression x) xs)

instance Show Expression where
  show = showExpression

-- simplify = fix (collapse . join . sort)


-- joining
fix :: (a -> a) -> a
fix f = f (fix f)

add :: Expression -> Expression -> Expression
add (Constant a) (Constant b) = Constant (a + b)
add (Constant a) expression = Sum ((Constant a):[expression])
add expression (Constant a) = Sum ((Constant a):[expression])
add expression1 expression2 = Sum [expression1, expression2]

myNegate :: Expression -> Expression
myNegate (Constant num) = Constant (-(num))
myNegate (Negate expression) = expression
myNegate expression = Negate expression

multiply :: Expression -> Expression -> Expression
multiply (Constant a) (Constant b) = Constant (a * b)
multiply (Constant a) (Negate (Constant b)) = Constant (negate (a * b))
multiply (Constant a) expression = Product [Constant a, expression]
multiply expression (Constant a) = multiply (Constant a) expression
multiply (Negate a) (Negate b) = multiply a b
multiply (Negate a) expression = Product [Negate a, expression]
multiply expression1 expression2 = Product [expression1, expression2]

distribute :: Expression -> Expression
distribute _ = undefined

-- simplifications --
simplify :: Expression -> Expression
simplify (Sum xs) = simplifySum (Sum (simplifyAll xs))
simplify (Product xs) = simplifyProduct (Product (simplifyAll xs))
simplify (Logarithm base (Product (xs))) = Sum (map (\x -> (Logarithm (simplify base) (simplify x))) (simplifyAll xs))
simplify (Constant a) = Constant a
simplify (Negate (Negate xs)) = simplify xs
simplify (Absolute (Negate xs)) = simplify xs
simplify (Absolute (Constant x)) = Constant x
simplify x = x

simplifyAll :: [Expression] -> [Expression]
simplifyAll = map (\x -> (simplify x))

simplifySum :: Expression -> Expression
simplifySum (Sum []) = (Constant 0)
simplifySum (Sum (x:[])) = (simplify x)
simplifySum (Sum ((Constant 0):xs)) = simplifySum (Sum xs)
simplifySum (Sum (x:(Constant 0):xs)) = simplifySum (Sum (x:xs))
simplifySum (Sum ((Sum xs):ys)) = simplifySum (Sum (xs ++ ys))
simplifySum (Sum (x:(Sum ys):zs)) = simplifySum (Sum (x:(ys++zs)))
simplifySum (Sum xs) = Sum xs

simplifyProduct :: Expression -> Expression
simplifyProduct (Product []) = (Constant 0)
simplifyProduct (Product (x:[])) = (simplify x)
simplifyProduct (Product ((Constant 0):xs)) = Constant 0
simplifyProduct (Product (x:(Constant 0):xs)) = Constant 0
simplifyProduct (Product ((Constant 1):xs)) = simplifyProduct (Product xs)
simplifyProduct (Product (x:(Constant 1):xs)) = simplifyProduct (Product (x:xs)) 
simplifyProduct (Product ((Sum xs):y:zs)) = Sum (map (\x -> simplify (Product (x:(y:zs)))) xs)
simplifyProduct (Product (x:(Sum ys):zs)) = Sum (map (\y -> simplify (Product (y:(x:zs)))) ys) 
simplifyProduct (Product ((Product xs):ys)) = simplify (Product (xs ++ ys))
simplifyProduct (Product (x:(Product ys):zs)) = simplify (Product (x:(ys ++ zs)))
simplifyProduct (Product xs) = (Product (simplifyAll xs))

-- collapsing --

collapseSum :: Expression -> Expression
collapseSum (Sum xs) = foldl add (Constant 0) xs

collapseProduct :: Expression -> Expression
collapseProduct (Product xs) = foldl multiply (Constant 1.0) xs

collapse :: Expression -> Expression
collapse (Sum xs) = collapseSum (simplify (Sum (flattenAll xs)))
collapse (Product xs) = collapseProduct (simplify (Product (flattenAll xs)))
collapse (Logarithm (Constant base) (Constant expr)) = Constant (logBase base expr)
collapse (Logarithm base expr) = (Logarithm (flatten base) (flatten expr))
collapse (Absolute expr) = (Absolute (flatten expr))
collapse (Negate expr) = (Negate (flatten expr))
collapse x = x 

collapseAll :: [Expression] -> [Expression]
collapseAll = map (\x -> collapse x)

flatten :: Expression -> Expression
flatten = simplify . collapse

flattenAll :: [Expression] -> [Expression]
flattenAll =  simplifyAll . collapseAll

solve :: Expression -> Expression
solve x 
  | (flatten x) == (flatten . flatten $ x) = (flatten x)
  | otherwise = solve (flatten x) 
-- evaluation --

evaluate :: Expression -> Double
evaluate (Constant value)             = value
evaluate (Absolute value)             = abs (evaluate value)
evaluate (Negate expression)          = -(evaluate expression)
evaluate (Sum expressions)            = foldr ((+) . evaluate) 0 expressions
evaluate (Product expressions)        = foldr ((*) . evaluate) 1 expressions
evaluate (Logarithm base expression)  = logBase (evaluate base) (evaluate expression)

-- rotations

-- expansions

-- equations

data Equation = Equation {
  lhs :: Expression,
  rhs :: Expression
}

instance (Show (Equation)) where
  show (Equation expr1 expr2) = (show expr1) ++ " = " ++ (show expr2)

valid :: Equation -> Bool
valid (Equation lhs rhs) = (evaluate lhs) == (evaluate rhs)