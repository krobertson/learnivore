module MathStructures
(
(|+|),
(|-|),
(|*|),
(|/|),
(|^|),
(|=|),
value,
ev,
val,
var,
neg,
ab,
lg,
nroot,
sqr,
sn,
cs,
tn,
tsec,
tcsc,
tcot,
nthRoot,
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
import Maybe

-- Data structure

data Term = Variable String 
          | Constant Double 
          | Integ Integer
          | Pi
          | E 
            deriving (Ord)
          
data UnaryOp = Negate 
             | Absolute
             | Sin
             | Cos
             | Tan
             | Sec
             | Csc
             | Cot
               deriving (Eq, Ord)
            
data BinaryOp = Logarithm 
              | Power
              | Multiply 
              | Divide 
              | Add 
              | Subtract
              | NthRoot
                deriving (Eq, Ord)
                
class UnaryOperator a where
  unOp :: a -> (Double -> Double)
  
class BinaryOperator a where
  binOp :: a -> (Double -> Double -> Double)
  
instance UnaryOperator UnaryOp where
  unOp Negate = negate
  unOp Absolute = abs
  unOp Sin = sin
  unOp Cos = cos
  unOp Tan = tan
  unOp Sec = (1 /) . sin
  unOp Csc = (1 /) . cos
  unOp Cot = (1 /) . tan
  
instance BinaryOperator BinaryOp where
  binOp Logarithm = logBase
  binOp Power = (**)
  binOp Multiply = (*)
  binOp Divide = (/)
  binOp Add = (+)
  binOp Subtract = (-)
  binOp NthRoot = flip nthRoot
  
class Evaluable a where
  ev ::  a -> a

class Valuable a where
  value :: a -> Maybe Double
  
instance Valuable Term where
  value Pi = Just pi
  value E = Just $ exp 1
  value (Integ x) = Just (fromInteger x)
  value (Constant x) = Just x
  value x = Nothing
  
instance Valuable Expression where
  value (Nullary term) = value term
  value x = Nothing
  
instance Evaluable Expression where
  ev ex@(Nullary x) = ex
  ev ex@(Unary op (Nullary x)) = fromMaybe ex (do v <- value x
                                                  Just (val . unOp op $ v))
  ev ex@(Unary _ _) = ex
  ev ex@(Binary op (Nullary x) (Nullary y)) = fromMaybe ex (do v1 <- value x
                                                               v2 <- value y
                                                               Just (val $ (binOp op) v1 v2))
  ev ex = ex
            
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
  Pi == Pi = True
  E == E = True
  _ == _ = False

instance Eq Expression where
  (Nullary term1) == (Nullary term2) = term1 == term2
  (Unary op expr) == (Unary op2 expr2) = op == op2 && expr == expr2
  (Binary op expr1 expr2) == (Binary op2 expr3 expr4) = op == op2 && expr1 == expr3 && expr2 == expr4
  _ == _ =  False  

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
ab = Unary Absolute
sqr = Binary NthRoot (val 2)
nroot = flip (Binary NthRoot)
lg = (Binary Logarithm)
var = Nullary . Variable
sn = Unary Sin
cs = Unary Cos
tn = Unary Tan
tsec = Unary Sec
tcsc = Unary Csc
tcot = Unary Cot

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

nthRoot x n = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)