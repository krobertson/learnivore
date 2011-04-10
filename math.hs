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

data Expression = Sum [Expression]
                | Subtract [Expression]
                | Product [Expression]
                | Divide [Expression]
                | Logarithm (Expression) (Expression)
                | Power (Expression) (Expression)
                | Absolute (Expression)
                | Negate (Expression)
                | Constant Double
                  deriving (Eq)

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

-- loadExpression :: String -> Expression
-- loadExpression = exprToExpression . getExpr . loadExpr

printExpression inp = case parse exprparser "" inp of
             { Left err -> return (Constant 0)
             ; Right ans -> return (exprToExpression $ ans)
             }

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

instance Show Expression where
  show = showExpression
  
data Solution = Solution [Expression]

showSolution :: Solution -> String
showSolution (Solution xs) = join "\n=>\n" (map show xs)

instance Show Solution where
  show = showSolution

br :: Tree a -> [a]
br t = map rootLabel $
      concat $
      takeWhile (not . null) $               
      iterate (concatMap subForest) [t]
-- 
-- solve :: Expression -> Solution
-- solve expression = takeWhile (not . (solved expression)) $
--                    br . transform $ 
--                    expression
--                    
-- solved :: Expression -> Expression -> Bool
-- solved x (Constant y) = 
printSolution :: Solution -> IO ()
printSolution = putStrLn . showSolution

-- transform :: Expression -> Tree Expression

-- merging
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
simplify (Subtract xs) = (Subtract (simplifyAll xs))
simplify (Product xs) = simplifyProduct (Product (simplifyAll xs))
simplify (Divide xs) = (Divide (simplifyAll xs))
simplify (Logarithm base (Product (xs))) = Sum (map (\x -> (Logarithm (simplify base) (simplify x))) (simplifyAll xs))
simplify (Power base expo) = Power (simplify base) (simplify expo)
simplify (Constant a) = Constant a
simplify (Negate (Negate xs)) = simplify xs
simplify (Absolute (Negate xs)) = simplify xs
simplify (Absolute (Constant x)) = Constant x
simplify x = x

simplifyAll :: [Expression] -> [Expression]
simplifyAll = map (\x -> (simplify x))

simplifySum :: Expression -> Expression
simplifySum (Sum []) = (Constant 0)
simplifySum (Sum (x:[])) = (simplify x)-- 
-- simplifySum (Sum ((Constant 0):xs)) = simplifySum (Sum xs)
-- simplifySum (Sum (x:(Constant 0):xs)) = simplifySum (Sum (x:xs))
simplifySum (Sum ((Sum xs):ys)) = simplifySum (Sum (xs ++ ys))
simplifySum (Sum (x:(Sum ys):zs)) = simplifySum (Sum (x:(ys++zs)))
simplifySum (Sum xs) = Sum xs

simplifyProduct :: Expression -> Expression
simplifyProduct (Product []) = (Constant 0)
simplifyProduct (Product (x:[])) = (simplify x)-- 
-- simplifyProduct (Product ((Constant 0):xs)) = Constant 0
-- simplifyProduct (Product (x:(Constant 0):xs)) = Constant 0
-- simplifyProduct (Product ((Constant 1):xs)) = simplifyProduct (Product xs)
-- simplifyProduct (Product (x:(Constant 1):xs)) = simplifyProduct (Product (x:xs)) 
-- simplifyProduct (Product ((Sum xs):y:zs)) = Sum (map (\x -> simplify (Product (x:(y:zs)))) xs)
-- simplifyProduct (Product (x:(Sum ys):zs)) = Sum (map (\y -> simplify (Product (y:(x:zs)))) ys) 
simplifyProduct (Product ((Product xs):ys)) = simplify (Product (xs ++ ys))
simplifyProduct (Product (x:(Product ys):zs)) = simplify (Product (x:(ys ++ zs)))
simplifyProduct (Product xs) = (Product (simplifyAll xs))

-- collapsing --

collapseSum :: Expression -> Expression
collapseSum (Sum xs) = foldl add (Constant 0) xs

collapseProduct :: Expression -> Expression
collapseProduct (Product xs) = foldl multiply (Constant 1.0) xs

collapse :: Expression -> Expression
collapse (Sum xs) = collapseSum (simplify (Sum (answerAll xs)))
collapse (Product xs) = collapseProduct (simplify (Product (answerAll xs)))
collapse (Logarithm (Constant base) (Constant expr)) = Constant (logBase base expr)
collapse (Logarithm base expr) = (Logarithm (answer base) (answer expr))
collapse (Absolute expr) = (Absolute (answer expr))
collapse (Negate expr) = (Negate (answer expr))
collapse x = x 

collapseAll :: [Expression] -> [Expression]
collapseAll = map (\x -> collapse x)

answer :: Expression -> Expression
answer = simplify . collapse

answerAll :: [Expression] -> [Expression]
answerAll =  simplifyAll . collapseAll

-- solve :: Expression -> Expression
-- solve x 
--   | (answer x) == (answer . answer $ x) = (answer x)
--   | otherwise = solve (answer x) 
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