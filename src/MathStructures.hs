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

-- Data structure
data Term = Var String 
          | Const Double 
          | In Integer
          
data UnaryOp = Neg 
             | Abs 
             | Par 
               deriving Eq
            
data BinaryOp = Log 
              | Pow
              | Mult 
              | Div 
              | Add 
              | Sub
                deriving Eq
            
data SeqOp = SigmaSum | PiProduct deriving Eq

data Expression = Nullary Term 
          | Unary UnaryOp (Expression) 
          | Binary BinaryOp (Expression) (Expression) 
          | Seq SeqOp [Expression]
            deriving Eq

data Equation = Equation Expression Expression
data Solution = Solution (Maybe [Expression]) 
data SolvedEquation = SolvedEquation (Maybe [Equation])

-- instance declarations
instance Eq Term where
  (Var x) == (Var y) = x == y
  (Const x) == (Const y) = x == y
  (In x) == (In y) = x == y
  (In x) == (Const y) = (fromInteger x) == y
  (Const x) == (In y) = x == (fromInteger y)
  _ == _ = False
  
instance Show Term where
  show (Var str) = str
  show (Const x) = show x
  show (In x) = show x

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
        | op == Pow = parenthesize $ showBinary operator leftExpression rightExpression
        | op == operator = showBinary operator leftExpression rightExpression
        | otherwise = parenthesize $ showBinary operator leftExpression rightExpression

showUnary :: UnaryOp -> Expression -> String
showUnary Neg x = "-" ++ parenthesize (show x)
showUnary Abs x = around (show x) "|" "|"
showUnary Par x = parenthesize $ show x

showBinary :: BinaryOp -> Expression -> Expression -> String
showBinary Add x y = inBetween (showExpression Add x) " + " (showExpression Add y)
showBinary Sub x y = inBetween (showExpression Sub x) " - " (showExpression Sub y)
showBinary Mult x y = inBetween (showExpression Mult x) " * " (showExpression Mult y)
showBinary Div x y = inBetween (showExpression Div x) " / " (showExpression Div y)
showBinary Log b x = "log" ++ angleBracket (show b) ++ parenthesize (show x)
showBinary Pow expr expo = inBetween (showExpression Pow expr) "^" (showExpression Pow expo)

showSeq = undefined

showSolution :: Solution -> String
showSolution (Solution (Just xs)) = join "\n=>\n" (map show xs)
showSolution (Solution Nothing) = "There is no valid solution"
  
showSolvedEquation :: SolvedEquation -> String
showSolvedEquation (SolvedEquation (Just xs)) = join "\n=>\n" (map show xs)
showSolvedEquation (SolvedEquation Nothing) = "There is no valid solution"

printEquation :: String -> IO ()
printEquation = putStrLn . processEquation id
             
printExpressionession :: String -> IO ()
printExpressionession = putStrLn . processExpression id

printExpression :: String -> IO ()
printExpression = putStrLn . processExpression id
  
-- parser wrappers
processExpression :: (Show a) => (Expression -> a) -> String -> String
processExpression fn inp = case parse exprparser "" inp of
                     { Left err -> "Not a legitimate Arithmetic Expressionession: " ++ show err
                     ; Right ans -> show . fn $ ans
                     }

processEquation :: (Show a) => (Equation -> a) -> String -> String
processEquation fn inp = case parse eqnparser "" inp of
                         { Left err -> show err
                         ; Right ans -> show . fn $ ans
                         } 
                         
-- traversal functions
-- exprFold1 :: (Expression -> a) -> Expression -> a
-- exprFold1 fn (Nullary term) = fn $ Nullary term
-- exprFold1 fn (Unary operator expr) = fn $ Unary operator (exprFold1 fn expr)
-- exprFold1 fn (Binary operator leftExpression rightExpression) = fn $ Binary operator (exprFold1 fn leftExpression) (exprFold1 fn rightExpression) 
-- 
-- exprMap :: (Expression -> Expression) -> Expression -> Expression
-- exprMap fn = exprFold1 fn

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
                 
termParser = liftM (Nullary . Var) m_identifier
             <|> liftM (Nullary . In) m_natural
             <|> liftM (Nullary . Const) m_float

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
           
