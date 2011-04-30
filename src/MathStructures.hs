module MathStructures
(
processExpression,
processEquation,
printEquation,
printExpression,
Expression(..),
Equation(..),
Solution(..),
SolvedEquation(..),
) where

import Data.List
import Control.Monad(liftM)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.Combinator

-- Data structure for actual calculation
data Expression = Variable String -- Product [(Variable "x"), (Integ 2)] = 2x
                | Constant Double
                | Integ Double
                | Absolute (Expression)
                | Negate (Expression)
                | Sum [Expression]
                | Subtract [Expression]
                | Product [Expression]
                | Divide [Expression]
                | Power (Expression) (Expression)
                | Logarithm (Expression) (Expression)
                  deriving (Eq, Ord)

data Equation = Equation {
  lhs :: Expression,
  rhs :: Expression
} deriving (Eq, Ord)

data Solution = Solution (Maybe [Expression]) 
data SolvedEquation = SolvedEquation (Maybe [Equation])

-- Data structures for parsing purposes
data UnaryOp = Neg | Abs | Par deriving Show
data BinaryOp = Mult | Div | Add | Sub | Log | Pow deriving Show

data Expr = Var String | Const Double | In Integer | Unary UnaryOp (Expr) | Binary BinaryOp (Expr) (Expr) | Seq [Expr] deriving Show
data Eqn = Eqn Expr Expr

-- instance declarations

instance Show Expression where
  show = showExpression

instance Show Solution where
  show = showSolution

instance (Show (Equation)) where
  show (Equation expr1 expr2) = (show expr1) ++ " = " ++ (show expr2)

instance Show SolvedEquation where
  show = showSolvedEquation

-- String and print functions

showExpression :: Expression-> String
showExpression (Product xs) = join " * " (map showExpression' xs)
showExpression (Divide xs) = join " / " (map showExpression' xs)
showExpression (Sum xs) = join " + " (map showExpression' xs)
showExpression (Subtract xs) = join " - " (map showExpression' xs)
showExpression expression = showExpression' expression

showExpression' :: Expression -> String
showExpression' (Variable string) = string
showExpression' (Constant a) = show a
showExpression' (Integ a) = show a
showExpression' (Negate a) = "-" ++ showExpression' a
showExpression' (Absolute a) = around (showExpression a) "|" "|"
showExpression' (Power a b) = showExpression' a ++ "^" ++ showExpression' b
showExpression' (Logarithm base a) = "log" ++ angleBracket (showExpression base) ++ parenthesize (showExpression a)
showExpression' expression = parenthesize $ showExpression expression

showSolution :: Solution -> String
showSolution (Solution (Just xs)) = join "\n=>\n" (map show xs)
showSolution (Solution Nothing) = "There is no valid solution"
  
showSolvedEquation :: SolvedEquation -> String
showSolvedEquation (SolvedEquation (Just xs)) = join "\n=>\n" (map show xs)
showSolvedEquation (SolvedEquation Nothing) = "There is no valid solution"

printEquation :: String -> IO ()
printEquation = putStrLn . processEquation id
             
printExpression :: String -> IO ()
printExpression = putStrLn . processExpression id
  
-- parser wrappers
processExpression :: (Show a) => (Expression -> a) -> String -> String
processExpression fn inp = case parse exprparser "" inp of
                         { Left err -> "Not a legitimate Arithmetic Expression: " ++ show err
                         ; Right ans -> show . fn $ exprToExpression ans
                         }
processEquation :: (Show a) => (Equation -> a) -> String -> String
processEquation fn inp = case parse eqnparser "" inp of
                         { Left err -> show err
                         ; Right ans -> show . fn $ eqnToEquation ans
                         } 
-- show helpers 

printBetween :: (Show a) => String -> [a] -> IO ()
printBetween str xs = print $ join str (map show xs)

join :: String -> [String] -> String
join str = concat . intersperse str

around :: String -> String -> String -> String
around obj start end = start ++ obj ++ end

parenthesize x = around x "(" ")"
angleBracket x = around x "<" ">"  

-- transformation of parsed objects into calculatable objects

exprToExpression :: Expr -> Expression
exprToExpression (Var str) = Variable str
exprToExpression (Const x) = Constant x
exprToExpression (In x) = Integ (fromInteger x :: Double)
exprToExpression (Unary Neg x) = Negate (exprToExpression x)
exprToExpression (Unary Abs x) = Absolute (exprToExpression x)
exprToExpression (Binary Pow x y) = Power (exprToExpression x) (exprToExpression y)
exprToExpression (Binary Mult x y) = Product [(exprToExpression x), (exprToExpression y)]
exprToExpression (Binary Div x y) = Divide [(exprToExpression x), (exprToExpression y)]
exprToExpression (Binary Add x y) = Sum [(exprToExpression x), (exprToExpression y)]
exprToExpression (Binary Sub x y) = Subtract [(exprToExpression x), (exprToExpression y)]
exprToExpression (Binary Log b x) = Logarithm (exprToExpression b) (exprToExpression x)

eqnToEquation :: Eqn -> Equation
eqnToEquation (Eqn expr1 expr2) = (Equation (exprToExpression expr1) (exprToExpression expr2))

-- parsers
eqnparser :: Parser Eqn
eqnparser = do { x <- exprparser
               ; string "="
               ; y <- exprparser
              ; return (Eqn x y)}

exprparser :: Parser Expr
exprparser = buildExpressionParser exprTable term <?> "expression"

exprTable = [ [Prefix (m_reservedOp "-" >> return (Unary Neg))]
            , [Infix (m_reservedOp "^" >> return (Binary Pow)) AssocLeft]
            , [Infix (m_reservedOp "*" >> return (Binary Mult)) AssocLeft]
            , [Infix (m_reservedOp "/" >> return (Binary Div)) AssocLeft]
            , [Infix (m_reservedOp "+" >> return (Binary Add)) AssocLeft]
            , [Infix (m_reservedOp "-" >> return (Binary Sub)) AssocLeft]
            ]
             
term = parenParser
       <|> logParser
       <|> absParser
       <|> termParser

within open close = do char open
                       x <- exprparser
                       char close
                       return x
                       
logParser = do string "log"
               base <- within '<' '>'
               expr <- within '(' ')'
               return (Binary Log base expr)
         
absParser = do x <- within '|' '|'
               return (Unary Abs x)

parenParser = do x <- m_parens exprparser
                 return (Unary Par x)
                 
termParser = liftM Var m_identifier
             <|> liftM In (m_natural)
             <|> liftM Const m_float

def = emptyDef{ identStart = letter
              , identLetter = oneOf $ ['1'..'9']
              , opStart = oneOf "|*-+=:^/"
              , opLetter = oneOf "|*-+=:^/"
              , reservedOpNames = ["*", "+", "-", "^", "/", "=", ":="]
              , reservedNames = ["log"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , float = m_float
           , natural = m_natural
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def
           
