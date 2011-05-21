module MathStructures
(
processExpression,
processEquation,
parseExpression,
parseEquation,
printEquation,
printExpression,
exmap,
fromOkEq,
topLevelExprs,
treeify,
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
import Text.JSON
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
              | NthRoot
                deriving (Eq, Ord)
            
data SeqOp = SigmaSum | PiProduct deriving (Eq, Ord)

data Expression = Nullary Term 
          | Unary UnaryOp (Expression) 
          | Binary BinaryOp (Expression) (Expression) 
          | Seq SeqOp [Expression]
            deriving (Eq, Ord)

data Equation = Equation Expression Expression deriving (Eq, Ord)
data Solution = Solution (Maybe [(String, Expression)]) 
data SolvedEquation = SolvedEquation (Maybe [(String, Equation)])

topLevelExprs op expr@(Binary op1 l r)
              | op == op1 = topLevelExprs' op expr
              | otherwise = []
topLevelExprs _ _ = []

topLevelExprs' op expr@(Binary op1 l r)
              | op1 == op = topLevelExprs' op l ++ topLevelExprs' op r
              | otherwise = [expr]
topLevelExprs' _ expr = [expr]

treeify :: BinaryOp -> [Expression] -> Expression
treeify op = foldl1 (\x y -> (Binary op x y))
                        
                       
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
  show NthRoot = "√"

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
  
instance JSON SolvedEquation where
  showJSON (SolvedEquation (Just xs)) = JSArray (map (\x -> JSArray [JSString . toJSString . fst $ x, showJSON (snd x)]) xs)
  readJSON json = makeSolvedEquation (fromOkVal (readJSON json))
  
instance JSON Expression where
  showJSON x = JSObject (toJSObject [("expression", JSString (toJSString . show $ x))])
  readJSON json= makeExpression (fromOk (readJSON json))
  
instance JSON Equation where
  showJSON (Equation lhs rhs) = JSObject (toJSObject [("equation", (JSObject (toJSObject [("lhs", showJSON lhs), ("rhs", showJSON rhs)])))]) 
  readJSON json = makeEquation (fromOk (readJSON json))
  

-- String, print, and json functions
makeExpression :: JSObject JSValue -> Result Expression
makeExpression json = let (!) = flip valFromObj in do
    expression <- (json ! "expression") :: Result JSString
    return (parseExpression . fromJSString $ expression)
    
makeEquation :: JSObject JSValue -> Result Equation
makeEquation json = let (!) = flip valFromObj in do
    equation <- (json ! "equation") :: Result (JSObject JSValue)
    lhs <- readJSON (JSObject (fromOk (equation ! "lhs"))) :: Result Expression
    rhs <- readJSON (JSObject (fromOk (equation ! "rhs"))) :: Result Expression
    return (Equation lhs rhs)
    
makeSolvedEquation :: [JSValue] -> Result SolvedEquation
makeSolvedEquation jsValues = return (SolvedEquation (Just (map (\x -> ("", (fromOkEq . readJSON) x)) jsValues)::Maybe [(String, Equation)]))
    
fromOk :: Result (JSObject JSValue) -> (JSObject JSValue)
fromOk (Text.JSON.Ok json) = json

fromOkEq :: Result Equation -> Equation
fromOkEq (Text.JSON.Ok a) = a
fromOkEq _ = (Equation (Nullary (Integ 0)) (Nullary (Integ 1)))

fromOkVal :: Result [JSValue] -> [JSValue]
fromOkVal (Text.JSON.Ok json) = json

fromOkGeneric (Text.JSON.Ok a) = a

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
showBinary NthRoot n x = angleBracket (show n) ++ "√" ++ parenthesize (show x)
showBinary op x y = inBetween (showExpression op x) (show op) (showExpression op y)

showSeq = undefined

joinFn :: (Show a) => (String -> String) -> [(String, a)] -> String 
joinFn fn [] = []
joinFn fn (x:[]) = show $ snd x
joinFn fn (x:xs) = joinFn fn [x] ++ joinFn' fn xs

joinFn' :: (Show a) => (String -> String) -> [(String, a)] -> String
joinFn' fn [] = []
joinFn' fn (x:[]) = fn (fst x) ++ (show $ snd x)
joinFn' fn (x:xs) = (joinFn' fn [x]) ++ joinFn' fn xs

showSolution :: Solution -> String
showSolution (Solution (Just xs)) = joinFn (\x -> "\n=> " ++ x ++ "\n") xs
showSolution (Solution Nothing) = "There is no valid solution"
  
showSolvedEquation :: SolvedEquation -> String
showSolvedEquation (SolvedEquation (Just xs)) = joinFn (\x -> "\n=> " ++ x ++ "\n") xs
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
                     
parseExpression :: String -> Expression
parseExpression inp = case parse exprparser "" inp of
                     { Left err -> (Binary Divide (Nullary (Integ 0)) (Nullary (Integ 0)))
                     ; Right ans -> ans
                     }

processEquation :: (Equation -> String) -> String -> String
processEquation fn inp = case parse eqnparser "" inp of
                         { Left err -> show err
                         ; Right ans -> fn $ ans
                         } 
                         
parseEquation :: String -> Equation
parseEquation inp = case parse eqnparser "" inp of
                         { Left err -> Equation (Nullary (Integ 0)) (Nullary (Integ 1))
                         ; Right ans -> ans
                         } 
                         
-- traversal functions
namedApply :: (String, (a -> b)) -> a -> (String, b)
namedApply fn x = (fst fn, snd fn $ x)

exmap :: (String, (Expression -> Expression)) -> Expression -> [(String, Expression)]
exmap fn (Nullary term) = [fn `namedApply` (Nullary term)]
exmap fn (Unary operator expr) = [fn `namedApply` (Unary operator expr)] ++ map (\x -> (fst x, Unary operator (snd x))) (exmap fn expr)
exmap fn (Binary operator leftExpression rightExpression) = [fn `namedApply` Binary operator leftExpression rightExpression]
                                                            ++ (map (\x -> (fst x, Binary operator leftExpression (snd x))) 
                                                                 (exmap fn rightExpression))
                                                            ++ (map (\x -> (fst x, Binary operator (snd x) rightExpression)) 
                                                                 (exmap fn leftExpression))


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
       <|> alternateRootParser
       <|> nthRootParser
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
               
nthRootParser = do n <- within '<' '>'
                   string "√"
                   expr <- within '(' ')'
                   return (Binary NthRoot n expr)
                   
alternateRootParser = do string "root"
                         n <- within '<' '>'
                         expr <- within '(' ')'
                         return (Binary NthRoot n expr)
         
absParser = do x <- within '|' '|'
               return (Unary Absolute x)

parenParser = do x <- m_parens exprparser
                 return (Unary Parens x)
                 
termParser = try floatParser <|> try varExprParser <|> intParser <|> varParser
             
floatParser = liftM (Nullary . Constant) m_float
intParser = liftM (Nullary . Integ) m_natural
varExprParser = do x <- (try floatParser) <|> intParser
                   var <- varParser
                   return (Binary Multiply x var)
varParser = liftM (Nullary . Variable) m_identifier

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