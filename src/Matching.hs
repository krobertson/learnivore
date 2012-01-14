module Matching
(
transEq,
trans,
bind,
match
) where
import Maybe
import Data.List
import MathStructures
import ReadAndWriteMathStructures

type Bindings = [(String, Expression)]

transEq :: String -> (Expression -> [Expression])
transEq eq = trans (parseEquation eq)

trans :: Equation -> Expression -> [Expression]
trans (Equation lhs rhs) expr = [expr] ++ (if bindingsL == [] then [] else [(bind bindingsL rhs)]) -- removed to prevent infinite growth ++ (if bindingsR == [] || not ((sort $ listOfVariables lhs) == (sort $ listOfVariables rhs)) then [] else [(bind bindingsR lhs)])
                                  where bindingsL = fromMaybe [] $ match lhs expr
                                        bindingsR = fromMaybe [] $ match rhs expr

bind :: Bindings -> Expression -> Expression
bind [] expr = expr
bind bindings expr@(Nullary (Variable x)) = fromMaybe expr $ binding x bindings
bind bindings expr@(Nullary x) = expr
bind bindings (Unary op expr) = (Unary op (bind bindings expr))
bind bindings (Binary op expr1 expr2) = (Binary op (bind bindings expr1) (bind bindings expr2))

binding :: String -> Bindings -> Maybe Expression
binding str bindings = do
                      (str, expr) <- find (\(string, expr) -> string == str) bindings
                      Just expr

match :: Expression -> Expression -> Maybe Bindings
match expr1 expr2
      | noDupBinding (fromMaybe [] bindings) = bindings
      | otherwise = Nothing
        where bindings = match' expr1 expr2
      
       
match' :: Expression -> Expression -> Maybe Bindings
match' expr1 expr2 = if (not (allTerms expr1)) then Nothing else (match'' (expr1,expr2))
                                                                                                                              
match'' (Nullary (Variable x), y) = Just [(x, y)]
match'' ((Nullary x), y)          = if (Nullary x) == y then Just [] else Nothing
match'' ((Unary op1 ex1), (Unary op2 ex2)) = if op1 == op2 then match ex1 ex2 else Nothing
match'' ((Unary _ _), x) = Nothing
match'' ((Binary op1 ex1 ex2), (Binary op2 ex3 ex4)) = if op1 == op2 then (do bindings <- match ex1 ex3;
                                                                              bindings2 <- match ex2 ex4;
                                                                              Just (nub $ bindings ++ bindings2)) else Nothing 
match'' ((Binary _ _ _), x) = Nothing
      

noDupBinding :: Bindings -> Bool
noDupBinding bindings = length bindings == (length $ nubBy (\x y -> fst x == fst y) bindings)


allTerms :: Expression -> Bool
allTerms (Nullary _) = True
allTerms (Unary  op expr) = allTerms expr
allTerms (Binary op ex1 ex2) = allTerms ex1 && allTerms ex2