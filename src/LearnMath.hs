module LearnMath
(renderEqSolution, renderEqSolutionJSON, renderAnswerJSON, solveEquation) where
import Data.String.Utils
import Text.JSON
import ReadAndWriteMathStructures
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
renderAnswerJSON solution question = encode . showJSON $ solveThroughEqs (process solution) (parseEquation question)
                                      where process = map parseEquation . split "," . replaceAll [("&lt;", "<"), ("&gt;", ">")]
                                            replaceAll = compose . map (\(x, y) -> (\xs -> replace x y xs))
                                            compose = foldr1 (.)