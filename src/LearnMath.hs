module LearnMath
(renderEqSolution, solveEquation) where
import Text.JSON
import MathStructures
import Expressions
import Equations
import Generators


-- This will ultimately be where the meat of Learnivore : Math appears
fromOk :: Result (JSObject JSValue) -> (JSObject JSValue)
fromOk (Text.JSON.Ok json) = json

fromOkEq :: Result Equation -> Equation
fromOkEq (Text.JSON.Ok a) = a
fromOkEq _ = (Equation (Nullary (Integ 0)) (Nullary (Integ 1)))

renderEqSolution = (processEquation (show . solveEq))
solveEqFromJSON json = show . solveEq . fromOkEq $ (Text.JSON.decode json :: Result Equation)