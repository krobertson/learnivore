module AlgebraicStructures
(
propagate,
associate,
associates,
commute,
commutes,
associative,
commutative
) where
import ReadAndWriteMathStructures
import MathStructures
import Data.List

propagate :: (Expression -> [Expression]) -> Expression -> [Expression]
propagate fn expr@(Nullary term) = fn expr
propagate fn expr@(Unary op ex) = fn expr ++ map (Unary op) (fn ex)
propagate fn expr@(Binary op ex1 ex2) = fn expr ++ [ (Binary op x ex2) | x <- propagate fn ex1] ++ [ Binary op ex1 y | y <- propagate fn ex2]

class Associates a where
  associate :: a -> [a]
  associates :: a -> Bool

instance Associates Expression where
  associate expr = nub $ propagate assoc expr
  associates (Binary op _ _) = associative op
  associates _ = False

associative op = op `elem` [Multiply, Add]

assoc :: Expression -> [Expression]
assoc expr1@(Binary op expr2@(Binary op2 xl yl) expr3@(Binary op3 xr yr))
      | associative op && op == op2 && op2 == op3 = [self, left, right, opleft, opright] 
      | associative op && op == op2 = [self, left]
      | associative op && op == op3 = [self, right]
      | otherwise = [self]
        where self = expr1
              left = (Binary op xl (Binary op yl expr3))
              opleft = (Binary op xl (Binary op (Binary op yl xr) yr))
              right = (Binary op (Binary op expr2 xr) yr)
              opright = (Binary op (Binary op xl (Binary op yl xr)) yr)
assoc expr@(Binary op (Binary op2 x y) z)
      | associative op && op == op2 = [(Binary op (Binary op x y) z), (Binary op x (Binary op y z))]
      | otherwise = [expr]
assoc expr@(Binary op x (Binary op2 y z))
      | associative op && op == op2 = [(Binary op (Binary op x y) z), (Binary op x (Binary op y z))]
      | otherwise = [expr]
assoc expr = [expr]



class Commutes a where
  commute :: a -> [a]
  commutes :: a -> Bool

instance Commutes Expression where
  commute expr = nub $ propagate comm expr
  commutes (Binary op _ _) = commutative op
  commutes _ = False

commutative op = op `elem` [Multiply, Add]

comm :: Expression -> [Expression]
comm expr@(Binary op expr1 expr2)
     | commutes expr = [(Binary op expr1 expr2), (Binary op expr2 expr1)]
     | otherwise = [expr]
comm expr = [expr]

