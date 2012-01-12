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
  (Nullary term1) == (Nullary term2) = term1 == term2
  (Unary op expr) == (Unary op2 expr2) = op == op2 && expr == expr2
  (Binary op expr1 expr2) == (Binary op2 expr3 expr4) = op == op2 && expr1 == expr3 && expr2 == expr4
  _ == _ =  False  
  
class SemanticEq a where
  (*==) :: a -> a -> Bool
  
instance SemanticEq Expression where
  (Nullary x) *== (Nullary y) = x == y
  (Unary opl l) *== (Unary opr r) = opl == opr && l *== r
  lhs@(Binary opl l r) *== rhs@(Binary opr ll rr)
        | isCommutative opl && opl == opr = (sort . process $ lhs) == (sort . process $ rhs)
        | otherwise = opl == opr && l == ll && (aProcess lhs) == (aProcess rhs) --(as long as the head is the same, should be able to freely rotate the tail)
          where process = (topLevelExprs opl)
                aProcess x = let processed = process x in 
                             if ifList processed then [(head processed)] ++ sort (tail processed) else processed 
                processedLhs = process lhs
                processedRhs = process rhs
                isCommutative = (`elem` [Add, Multiply])
                ifList = (> 1) . length
  _ *== _ = False

-- constructor helpers
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

bopConstructor Add = (|+|)
bopConstructor Subtract = (|-|)
bopConstructor Multiply = (|*|)
bopConstructor Divide = (|/|)
bopConstructor Power = (|^|)
bopConstructor NthRoot = flip nroot
bopConstructor Logarithm = lg

-- expression manipulation

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

exmap :: (String, (Expression -> [Expression])) -> Expression -> [(String, Expression)]
exmap fn expression = map (\x -> (fst result, x)) $ snd result
                        where result = (fn `namedApply` expression)