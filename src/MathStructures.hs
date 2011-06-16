module MathStructures
(
(|+|),
(|-|),
(|*|),
(|/|),
(|^|),
(|=|),
val,
var,
neg,
par,
ab,
lg,
nroot,
sqr,
bopConstructor,
exmap,
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
            deriving (Ord)

data Equation = Equation {lhs::Expression, rhs::Expression} deriving (Eq, Ord)
data Solution = Solution (Maybe ([Expression], [String])) 
data SolvedEquation = SolvedEquation (Maybe ([Equation], [String]))
                        
                       
-- instance declarations
instance Eq Term where
  (Variable x) == (Variable y) = x == y
  (Constant x) == (Constant y) = x == y
  (Integ x) == (Integ y) = x == y
  (Integ x) == (Constant y) = (fromInteger x) == y
  (Constant x) == (Integ y) = x == (fromInteger y)
  _ == _ = False

instance Eq Expression where
  (Nullary x) == (Nullary y) = x == y
  (Unary opl l) == (Unary opr r) = opl == opr && l == r
  lhs@(Binary opl l r) == rhs@(Binary opr ll rr)
          | isCommutative opl && opl == opr = (sort . process $ lhs) == (sort . process $ rhs)
          | otherwise = opl == opr && l == ll && (aProcess lhs) == (aProcess rhs) --(as long as the head is the same, should be able to freely rotate the tail)
            where process = (topLevelExprs opl)
                  aProcess x = let processed = process x in 
                               if ifList processed then [(head processed)] ++ sort (tail processed) else processed 
                  processedLhs = process lhs
                  processedRhs = process rhs
                  isCommutative = (`elem` [Add, Multiply])
                  ifList = (> 1) . length
  x == y = False 

-- construction helpers
infixl 5 |=|
infixl 8 |^|
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
infixl 7 |/|

(|+|) = Binary Add
(|-|) = Binary Subtract
(|*|) = Binary Multiply
(|/|) = Binary Divide
(|^|) = Binary Power
(|=|) = Equation

bopConstructor Add = (|+|)
bopConstructor Subtract = (|-|)
bopConstructor Multiply = (|*|)
bopConstructor Divide = (|/|)
bopConstructor Power = (|^|)
bopConstructor NthRoot = nroot
bopConstructor Logarithm = lg


val :: Double -> Expression
val x
		| fromInteger (round x) == x = Nullary (Integ (round x))
		| otherwise = Nullary (Constant x)
		
neg = Unary Negate
par = Unary Parens
ab = Unary Absolute
sqr = Binary NthRoot (val 2)
nroot = flip (Binary NthRoot)
lg = (Binary Logarithm)
var = Nullary . Variable

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


 