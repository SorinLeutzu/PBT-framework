module Main where

import Matchers.Core
import Shrinking.Core (shrink)
import TestTree.Types
import TestTree.Builders
import TestTree.Runner (executeTestTree, countResults, collectLeaves)

report :: String -> [(String, Bool)] -> IO ()
report title checks = do
  putStrLn ("== " ++ title ++ " ==")
  mapM_ (\(name, ok) -> putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)) checks
  let total  = length checks
      passed = length (filter snd checks)
  putStrLn (replicate 50 '-')
  putStrLn ("Passed " ++ show passed ++ " / " ++ show total)

flattenResults :: TestResultTree -> [(String, TestOutcome)]
flattenResults (ResultLeaf name outcome) = [(name, outcome)]
flattenResults (ResultNode _ children)   = concatMap flattenResults children

passing1, passing2, passing3 :: TestTree
passing1 = group AllPass "p1" [ testCase 100 "eq" (EqMatcher (1 :: Int)) (1 :: Int) ]
passing2 = group AllPass "p2"
  [ testCase 50 "gt" (GtMatcher (0 :: Int)) (5 :: Int)
  , testCase 50 "lt" (LtMatcher (10 :: Int)) (5 :: Int) ]
passing3 = group AllPass "p3" [ testCase 100 "contains" (ContainsMatcher "ab") "abc" ]

failing1, failing2, failing3 :: TestTree
failing1 = group AllPass "f1" [ testCase 100 "eq" (EqMatcher (1 :: Int)) (2 :: Int) ]
failing2 = group AllPass "f2"
  [ testCase 50 "eq" (EqMatcher (1 :: Int)) (2 :: Int)
  , testCase 50 "gt" (GtMatcher (10 :: Int)) (5 :: Int) ]
failing3 = group AllPass "f3" [ testCase 100 "contains" (ContainsMatcher "zz") "abc" ]

nested1, nested2, nested3 :: TestTree
nested1 = group AllPass "n1"
  [ group AllPass "ok"  [ testCase 100 "p" (EqMatcher (1 :: Int)) (1 :: Int) ]
  , group AllPass "bad" [ testCase 100 "f" (EqMatcher (1 :: Int)) (2 :: Int) ] ]
nested2 = group AllPass "n2"
  [ group AllPass "g"
      [ testCase 50 "p1" (EqMatcher (1 :: Int)) (1 :: Int)
      , group AllPass "h" [ testCase 50 "p2" (EqMatcher (2 :: Int)) (2 :: Int) ] ]
  , testCase 50 "f1" (EqMatcher (3 :: Int)) (4 :: Int) ]
nested3 = group AllPass "n3"
  [ group AllPass "a" [ testCase 100 "p" (GtMatcher (0 :: Int)) (9 :: Int) ]
  , group AllPass "b"
      [ testCase 50 "p" (LtMatcher (5 :: Int)) (1 :: Int)
      , testCase 50 "f" (LtMatcher (1 :: Int)) (9 :: Int) ] ]

fuzzTree :: TestTree
fuzzTree = group AllPass "fuzz"
  [ fuzz 100 "(+) commutative" (Commutativity ((+) :: Int -> Int -> Int))
      (shrink :: (Int, Int) -> [(Int, Int)]) 50 ]

reproducible :: TestTree -> Bool
reproducible t =
  let r1 = flattenResults (executeTestTree t)
      r2 = flattenResults (executeTestTree t)
      r3 = flattenResults (executeTestTree t)
  in r1 == r2 && r2 == r3

reproducibilityChecks :: [(String, Bool)]
reproducibilityChecks =
  [ ("reproducible: nested1", reproducible nested1)
  , ("reproducible: fuzzTree", reproducible fuzzTree)
  , ("reproducible: nested3", reproducible nested3)
  ]

leafCountChecks :: [(String, Bool)]
leafCountChecks =
  [ (name ++ ": passed + failed == leaves", p + f == length (collectLeaves t))
  | (name, t) <- [ ("passing2", passing2), ("failing2", failing2)
                 , ("nested1", nested1), ("nested2", nested2), ("nested3", nested3) ]
  , let (p, f) = countResults (executeTestTree t) ]

counts :: TestTree -> (Int, Int)
counts = countResults . executeTestTree

unitChecks :: [(String, Bool)]
unitChecks =
  [ ("passing1 -> (1,0)", counts passing1 == (1, 0))
  , ("passing2 -> (2,0)", counts passing2 == (2, 0))
  , ("passing3 -> (1,0)", counts passing3 == (1, 0))
  , ("failing1 -> (0,1)", counts failing1 == (0, 1))
  , ("failing2 -> (0,2)", counts failing2 == (0, 2))
  , ("failing3 -> (0,1)", counts failing3 == (0, 1))
  , ("nested1 -> (1,1)", counts nested1 == (1, 1))
  , ("nested2 -> (2,1)", counts nested2 == (2, 1))
  , ("nested3 -> (2,1)", counts nested3 == (2, 1))
  ]

main :: IO ()
main = report "6.1.5 Test tree"
  (reproducibilityChecks ++ leafCountChecks ++ unitChecks)