module TestAlgebraicStructures
(algebraicStructureTests) where
  
import ReadAndWriteMathStructures
import Data.List
import MathStructures
import AlgebraicStructures
import Test.HUnit

algebraicStructureTests = TestLabel "Algebraic Structure Tests" (TestList [associativityTests, commutativityTests])
associativityTests = TestLabel "Associativity Tests" (TestList [tAssocAddShallow, tAssocAddNested, tAssocAddDeep])
commutativityTests = TestLabel "Commutativity Tests" (TestList [tCommAddShallow, tCommAddNested, tCommAddDeep])

tAssocAddShallow = TestCase $ assertEqual
          "should associate 3 integers"
          (sort $ associate ((val 2) |+| (val 3) |+| (val 4))) $
          sort [(val 2) |+| ((val 3) |+| (val 4)), ((val 2) |+| (val 3)) |+| (val 4)]
          
tAssocAddNested = TestCase $ assertEqual
          "should associate 4 integers"
          (sort $ associate (((val 2) |+| (val 3)) |+| ((val 4) |+| (val 5)))) $
          sort [
                  val 2 |+| (val 3 |+| (val 4 |+| val 5)),
                  val 2 |+| ((val 3 |+| val 4) |+| val 5),
                  (val 2 |+| val 3) |+| (val 4 |+| val 5),
                  (val 2 |+| (val 3 |+| val 4)) |+| val 5,
                  ((val 2 |+| val 3) |+| val 4) |+| val 5
               ]
               
tAssocAddDeep = TestCase $ assertEqual
               "should associate 3 integers underneath a separate operator"
               (sort $ associate ((val 1) |*| ((val 2) |+| (val 3) |+| (val 4)))) $
               sort (map (val 1 |*|) [(val 2) |+| ((val 3) |+| (val 4)), ((val 2) |+| (val 3)) |+| (val 4)])
               
tCommAddShallow = TestCase $ assertEqual
                  "should commute 2 integers"
                  (sort . commute $ (val 1) |*| (val 2))
                  (sort [ val 1 |*| val 2, val 2 |*| val 1])
                  
tCommAddNested = TestCase $ assertEqual
             "should commute 4 integers"
             (sort . commute $ (val 2 |+| val 3) |+| (val 4 |+| val 5)) $
             sort [
                     (val 2 |+| val 3) |+| (val 4 |+| val 5),
                     (val 3 |+| val 2) |+| (val 4 |+| val 5),
                     (val 2 |+| val 3) |+| (val 5 |+| val 4),
                     (val 4 |+| val 5) |+| (val 2 |+| val 3)
                  ]

tCommAddDeep = TestCase $ assertEqual
              "should commute 2 integers underneath a separate operator"
              (sort . commute $ val 1 |*| (val 2 |+| val 3)) $
              sort [
                val 1 |*| (val 2 |+| val 3),
                val 1 |*| (val 3 |+| val 2),
                (val 2 |+| val 3) |*| val 1             
              ]



