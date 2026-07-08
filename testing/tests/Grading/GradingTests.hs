module Main where

import Matchers.Core
import TestTree.Types
import TestTree.Builders
import Grading (Grade(..), gradeTree, resultGrade)

report :: String -> [(String, Bool)] -> IO ()
report title checks = do
  putStrLn ("== " ++ title ++ " ==")
  mapM_ (\(name, ok) -> putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)) checks
  let total  = length checks
      passed = length (filter snd checks)
  putStrLn (replicate 50 '-')
  putStrLn ("Passed " ++ show passed ++ " / " ++ show total)

gradeValue :: TestTree -> Double
gradeValue t = let Grade g = resultGrade (gradeTree t) in g

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 0.001

mkTree :: Int -> TestTree
mkTree k = group AllPass "mono" [ leaf i | i <- [1 .. 4] ]
  where
    leaf i = testCase 25 ("t" ++ show i) (EqMatcher (i :: Int))
                       (if i <= k then (-1) else i)

monotonicityChecks :: [(String, Bool)]
monotonicityChecks =
  [ (show k ++ " test(s) fail->pass does not lower the grade",
       gradeValue (mkTree 0) >= gradeValue (mkTree k))
  | k <- [1, 2, 3] ]

gt1 :: TestTree
gt1 = group AllPass "g1" [ testCase 25 ("t" ++ show i) (EqMatcher (i :: Int)) i | i <- [1 .. 4] ]

gt2 :: TestTree
gt2 = group AllPass "g2"
  [ testCase 25 "a" (EqMatcher (1 :: Int)) (1 :: Int)
  , testCase 25 "b" (EqMatcher (2 :: Int)) (2 :: Int)
  , testCase 25 "c" (EqMatcher (3 :: Int)) (0 :: Int)
  , testCase 25 "d" (EqMatcher (4 :: Int)) (0 :: Int) ]

gt3 :: TestTree
gt3 = group AllPass "g3"
  [ testCase 50 "a" (EqMatcher (1 :: Int)) (0 :: Int)
  , testCase 50 "b" (EqMatcher (2 :: Int)) (0 :: Int) ]

gt4 :: TestTree
gt4 = group AllPass "g4"
  [ group AllPass "a"
      [ testCase 50 "a1" (EqMatcher (1 :: Int)) (1 :: Int)
      , testCase 50 "a2" (EqMatcher (2 :: Int)) (2 :: Int) ]
  , group AllPass "b"
      [ testCase 50 "b1" (EqMatcher (3 :: Int)) (3 :: Int)
      , testCase 50 "b2" (EqMatcher (4 :: Int)) (0 :: Int) ] ]

unitChecks :: [(String, Bool)]
unitChecks =
  [ ("gt1 grade == 100", approx (gradeValue gt1) 100)
  , ("gt2 grade == 50",  approx (gradeValue gt2) 50)
  , ("gt3 grade == 0",   approx (gradeValue gt3) 0)
  , ("gt4 grade == 75",  approx (gradeValue gt4) 75)
  ]

main :: IO ()
main = report "6.1.6 Grading" (monotonicityChecks ++ unitChecks)