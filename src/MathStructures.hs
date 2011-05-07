module MathStructures
(
processExpression,
processEquation,
printEquation,
printExpression,
exmap,
Term(..),
UnaryOp(..),
BinaryOp(..),
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

-- Data structure
data Term = Variable String 
          | Constant Double 
          | Integ Integer
            deriving (Ord)
          
data UnaryOp = Negate 
             | Absolute 
             | Parens 
               deriving (Eq, Ord)
            
data BinaryOp = Logarithm 
              | Power
              | Multiply 
              | Divide 
              | Add 
              | Subtract
                deriving (Eq, Ord)
            
data SeqOp = SigmaSum | PiProduct deriving (Eq, Ord)

data Expression = Nullary Term 
          | Unary UnaryOp (Expression) 
          | Binary BinaryOp (Expression) (Expression) 
          | Seq SeqOp [Expression]
            deriving (Eq, Ord)

data Equation = Equation Expression Expression deriving (Eq, Ord)
data Solution = Solution (Maybe [Expression]) 
data SolvedEquation = SolvedEquation (Maybe [Equation])

-- instance declarations
instance Eq Term where
  (Variable x) == (Variable y) = x == y
  (Constant x) == (Constant y) = x == y
  (Integ x) == (Integ y) = x == y
  (Integ x) == (Constant y) = (fromInteger x) == y
  (Constant x) == (Integ y) = x == (fromInteger y)
  _ == _ = False
  
instance Show Term where
  show (Variable str) = str
  show (Constant x) = show x
  show (Integ x) = show x
  
instance Show BinaryOp where
  show Add = " + "
  show Subtract = " - "
  show Multiply = " * "
  show Divide = " / "
  show Power = "^"
  show Logarithm = "log"

instance Show Expression where
  show (Nullary term) = show term
  show (Unary op expr) = showUnary op expr
  show (Binary op leftExpression rightExpression) = showBinary op leftExpression rightExpression 

instance Show Solution where
  show = showSolution

instance (Show (Equation)) where
  show (Equation expr1 expr2) = (show expr1) ++ " = " ++ (show expr2)

instance Show SolvedEquation where
  show = showSolvedEquation

-- String and print functions
showExpression :: BinaryOp -> Expression -> String
showExpression _ (Nullary term) = show term
showExpression _ (Unary operator expr) = showUnary operator expr
showExpression op (Binary operator leftExpression rightExpression)
        | op == Power = parenthesize $ showBinary operator leftExpression rightExpression
        | op == operator = showBinary operator leftExpression rightExpression
        | otherwise = parenthesize $ showBinary operator leftExpression rightExpression

showUnary :: UnaryOp -> Expression -> String
showUnary Negate x = "-" ++ parenthesize (show x)
showUnary Absolute x = around (show x) "|" "|"
showUnary Parens x = parenthesize $ show x

showBinary :: BinaryOp -> Expression -> Expression -> String
showBinary Logarithm b x = "log" ++ angleBracket (show b) ++ parenthesize (show x)
showBinary op x y = inBetween (showExpression op x) (show op) (showExpression op y)

showSeq = undefined

showSolution :: Solution -> String
showSolution (Solution (Just xs)) = join "\n=>\n" (map show xs)
showSolution (Solution Nothing) = "There is no valid solution"
  
showSolvedEquation :: SolvedEquation -> String
showSolvedEquation (SolvedEquation (Just xs)) = join "\n=>\n" (map show xs)
showSolvedEquation (SolvedEquation Nothing) = "There is no valid solution"

printEquation :: String -> IO ()
printEquation = putStrLn . processEquation (show . id)
             
printExpressionession :: String -> IO ()
printExpressionession = putStrLn . processExpression (show . id)

printExpression :: String -> IO ()
printExpression = putStrLn . processExpression (show . id)
  
-- parser wrappers
processExpression :: (Expression -> String) -> String -> String
processExpression fn inp = case parse exprparser "" inp of
                     { Left err -> "Not a legitimate Arithmetic Expressionession: " ++ show err
                     ; Right ans -> fn $ ans
                     }

processEquation :: (Equation -> String) -> String -> String
processEquation fn inp = case parse eqnparser "" inp of
                         { Left err -> show err
                         ; Right ans -> fn $ ans
                         } 
                         
-- traversal functions
exmap :: (Expression -> Expression) -> Expression -> Expression
exmap fn (Nullary term) = fn $ Nullary term
exmap fn (Unary operator expr) = fn $ Unary operator (exmap fn expr)
exmap fn (Binary operator leftExpression rightExpression) = fn $ Binary operator (exmap fn leftExpression) (exmap fn rightExpression) 

-- show helpers 

printBetween :: (Show a) => String -> [a] -> IO ()
printBetween str xs = print $ join str (map show xs)

join :: String -> [String] -> String
join str = concat . intersperse str

around :: String -> String -> String -> String
around obj start end = start ++ obj ++ end

inBetween :: String -> String -> String -> String
inBetween left inside right = left ++ inside ++ right

parenthesize x = around x "(" ")"
angleBracket x = around x "<" ">"  

-- transformation of parsed objects into calculatable objects

-- parsers
eqnparser :: Parser Equation
eqnparser = do { x <- exprparser
               ; string "="
               ; y <- exprparser
              ; return (Equation x y)}

exprparser :: Parser Expression
exprparser = buildExpressionParser exprTable term <?> "expression"

exprTable = [ [Prefix (m_reservedOp "-" >> return (Unary Negate))]
            , [Infix (m_reservedOp "^" >> return (Binary Power)) AssocLeft]
            , [Infix (m_reservedOp "*" >> return (Binary Multiply)) AssocLeft]
            , [Infix (m_reservedOp "/" >> return (Binary Divide)) AssocLeft]
            , [Infix (m_reservedOp "+" >> return (Binary Add)) AssocLeft]
            , [Infix (m_reservedOp "-" >> return (Binary Subtract)) AssocLeft]
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
               return (Binary Logarithm base expr)
         
absParser = do x <- within '|' '|'
               return (Unary Absolute x)

parenParser = do x <- m_parens exprparser
                 return (Unary Parens x)
                 
termParser = liftM (Nullary . Variable) m_identifier
             <|> liftM (Nullary . Integ) m_natural
             <|> liftM (Nullary . Constant) m_float

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
           
