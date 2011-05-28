module LearnMath
(renderEqSolution, renderEqSolutionJSON, renderAnswerJSON, solveEquation) where
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
renderEqSolutionJSON json = encode . showJSON . solveEq . parseEquation $ json

renderAnswerJSON :: String -> String -> String
renderAnswerJSON solution question = solution ++ "   " ++ question--encode . showJSON . solveThroughEqs (parseSolution solution) $ (parseEquation question)
																			--where parseSolution solution = [solution]