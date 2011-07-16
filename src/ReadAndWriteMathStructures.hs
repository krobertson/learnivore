module ReadAndWriteMathStructures 
(
processExpression,
processEquation,
parseExpression,
parseEquation,
printEquation,
printExpression,
fromOkEq
)
where
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
import MathStructures

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
 showJSON (SolvedEquation (Just (xs, ys))) = JSArray (zipWith (\x y -> JSArray [JSString . toJSString $ x, showJSON y]) ys xs)
 readJSON json = Text.JSON.Ok (SolvedEquation Nothing) --makeSolvedEquation (fromOkVal (readJSON json))

instance JSON Expression where
 showJSON x = JSObject (toJSObject [("expression", JSString (toJSString . show $ x))])
 readJSON json= makeExpression (fromOk (readJSON json))

instance JSON Equation where
 showJSON (Equation lhs rhs) = JSObject (toJSObject [("equation", (JSObject (toJSObject [("lhs", showJSON lhs), ("rhs", showJSON rhs)])))]) 
 readJSON json = makeEquation (fromOk (readJSON json))

printEquation :: String -> IO ()
printEquation = putStrLn . processEquation (show . id)

printExpression :: String -> IO ()
printExpression = putStrLn . processExpression (show . id)

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

-- parser wrappers
processExpression :: (Expression -> String) -> String -> String
processExpression fn inp = case parse exprparser "" inp of
                     { Left err -> "Not a legitimate Arithmetic Expressionession: " ++ show err
                     ; Right ans -> fn $ ans
                     }
                     
parseExpression :: String -> Expression
parseExpression inp = case parse exprparser "" inp of
                     { Left err -> val 0 |/| val 0
                     ; Right ans -> ans
                     }

processEquation :: (Equation -> String) -> String -> String
processEquation fn inp = case parse eqnparser "" inp of
                         { Left err -> show err
                         ; Right ans -> fn $ ans
                         } 
                         
parseEquation :: String -> Equation
parseEquation inp = case parse eqnparser "" inp of
                         { Left err -> val 0 |=| val 1
                         ; Right ans -> ans
                         }

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
    return (lhs |=| rhs)

makeSolvedEquation :: [JSValue] -> Result SolvedEquation
makeSolvedEquation jsValues = return (SolvedEquation (Just (sol, map (\x -> "") sol)))
								where sol = (map (fromOkEq . readJSON) jsValues)::[Equation]

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
showUnary Negate (Nullary x) = "-" ++ (show x)
showUnary Negate x = "-" ++ parenthesize (show x)
showUnary Absolute x = around (show x) "|" "|"
showUnary Parens x = parenthesize $ show x

showBinary :: BinaryOp -> Expression -> Expression -> String
showBinary Logarithm b x = "log" ++ angleBracket (show b) ++ parenthesize (show x)
showBinary NthRoot n x = angleBracket (show n) ++ "√" ++ parenthesize (show x)
showBinary op x y = inBetween (showExpression op x) (show op) (showExpression op y)

showSeq = undefined

joinFn :: (Show a) => (String -> String) -> ([a], [String]) -> String 
joinFn fn ([], []) = []
joinFn fn (x:[], y:[]) = show x
joinFn fn (x:xs, y:ys) = joinFn fn ([x], [y]) ++ joinFn' fn (xs, ys)

joinFn' :: (Show a) => (String -> String) -> ([a], [String]) -> String
joinFn' fn ([], []) = []
joinFn' fn (x:[], y:[]) = fn y ++ (show x)
joinFn' fn (x:xs, y:ys) = (joinFn' fn ([x],[y])) ++ joinFn' fn (xs, ys)

showSolution :: Solution -> String
showSolution (Solution (Just xs)) = joinFn (\x -> "\n=> " ++ x ++ "\n") xs
showSolution (Solution Nothing) = "There is no valid solution"

showSolvedEquation :: SolvedEquation -> String
showSolvedEquation (SolvedEquation (Just xs)) = joinFn (\x -> "\n=> " ++ x ++ "\n") xs
showSolvedEquation (SolvedEquation Nothing) = "There is no valid solution"

-- parsers
eqnparser :: Parser Equation
eqnparser = do { x <- exprparser
               ; string "="
               ; y <- exprparser
              ; return (x |=| y)}

exprparser :: Parser Expression
exprparser = buildExpressionParser exprTable term <?> "expression"

exprTable = [ [Prefix (m_reservedOp "-" >> return neg)]
            , [Infix (m_reservedOp "^" >> return (|^|)) AssocLeft]
            , [Infix (m_reservedOp "*" >> return (|*|)) AssocLeft, Infix (m_reservedOp "/" >> return (|/|)) AssocLeft]
            , [Infix (m_reservedOp "+" >> return (|+|)) AssocLeft, Infix (m_reservedOp "-" >> return (|-|)) AssocLeft]
            ]

term = parenParser
       <|> logParser
			 <|> squareRootParser
			 <|> alternateSquareRootParser
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
               return (lg base expr)

nthRootParser = do n <- within '<' '>'
                   string "√" <|> string "root"
                   expr <- within '(' ')'
                   return (expr `nroot` n)

alternateRootParser = do string "root"
                         n <- within '<' '>'
                         expr <- within '(' ')'
                         return (expr `nroot` n)

squareRootParser = do string "√"
                      expr <- within '(' ')'
                      return (sqr expr)

alternateSquareRootParser = do string "sqrt"
                               expr <- within '(' ')'
                               return (sqr expr)

absParser = do x <- within '|' '|'
               return (ab x)

parenParser = do x <- within '(' ')'
                 return (par x)

termParser = try floatParser <|> try varExprParser <|> intParser <|> varParser

floatParser = liftM val m_float

intParser = liftM (val . fromInteger) m_natural

varExprParser = do x <- (try floatParser) <|> intParser
                   v <- varParser
                   return (x |*| v)
varParser = liftM var m_identifier

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