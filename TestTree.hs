module TestTree
  ( module TestTree.Types,
    module TestTree.Builders,
    module TestTree.Runner,
    exampleTree,
    exampleTreeFailing,
    exampleTreeNested,
    main,
  )
where

import Matchers.Core
import Shrinking.Core qualified as Shrink
import PrettyPrinting.TerminalSetup (setupTerminalWithChcp)
import TestTree.Types
import TestTree.Builders
import TestTree.Runner


-- example trees

exampleTree :: TestTree
exampleTree =
  group AllPass
    "Root"
    [ group AllPass
        "Strings"
        [ testCase 25.0 "Contains \"ell\" in \"hello\"" (ContainsMatcher "ell") "hello",
          testCase 25.0 "Starts with \"he\"" (StartsWithMatcher "he") "hello",
          testCase 25.0 "Ends with \"lo\"" (EndsWithMatcher "lo") "hello",
          testCase 25.0 "Equality 5 == 5" (EqMatcher (5 :: Int)) (5 :: Int)
        ],
      group AllPass
        "Arithmetic properties (Int)"
        [ fuzz 33.33 "(+): associativity on triples" (Associativity ((+) :: Int -> Int -> Int)) (Shrink.shrink :: (Int, Int, Int) -> [(Int, Int, Int)]) 200,
          fuzz 33.33 "(+): commutativity on pairs" (Commutativity ((+) :: Int -> Int -> Int)) (Shrink.shrink :: (Int, Int) -> [(Int, Int)]) 200,
          fuzz
            33.34
            "(* distributes over +)"
            (Distributivity ((+) :: Int -> Int -> Int) ((*) :: Int -> Int -> Int))
            (Shrink.shrink :: (Int, Int, Int) -> [(Int, Int, Int)])
            200
        ],
      group AllPass
        "Simple matcher with fuzzed single Int"
        [ fuzz 100.0 "gt 0 holds for positive Ints (may fail)" (gt (0 :: Int)) (Shrink.shrink :: Int -> [Int]) 100
        ]
    ]

exampleTreeFailing :: TestTree
exampleTreeFailing =
  group AllPass
    "Root (Failing examples)"
    [ group AllPass
        "Int properties (expected to fail)"
        [ fuzz 50.0 "equals 5 for random Ints (expected to fail)" (EqMatcher (5 :: Int)) (Shrink.shrink :: Int -> [Int]) 100,
          fuzz 50.0 "(-) associative on triples (expected to fail)" (Associativity ((-) :: Int -> Int -> Int)) (Shrink.shrink :: (Int, Int, Int) -> [(Int, Int, Int)]) 100
        ],
      group AllPass
        "String properties (expected to fail)"
        [ fuzz 50.0 "contains \"xyz\" (expected to fail)" (ContainsMatcher "xyz") (Shrink.shrink :: String -> [String]) 100,
          fuzz 50.0 "starts with \"hello\" (expected to fail)" (StartsWithMatcher "hello") (Shrink.shrink :: String -> [String]) 100
        ],
      group AllPass
        "Unit tests (failing)"
        [ testCase 100.0 "Equality 5 == 42 (expected to fail)" (EqMatcher (5 :: Int)) (42 :: Int)
        ]
    ]

exampleTreeNested :: TestTree
exampleTreeNested =
  group AllPass
    "Root"
    [ group AllPass
        "Level 1 - Group A (50%)"
        [ testCase 50.0 "Test A1: Simple pass" (EqMatcher (1 :: Int)) (1 :: Int),
          group AllPass
            "Level 2 - Subgroup A1 (50%)"
            [ testCase 50.0 "Test A1.1: Nested pass" (EqMatcher (2 :: Int)) (2 :: Int),
              testCase 50.0 "Test A1.2: Nested pass" (EqMatcher (3 :: Int)) (3 :: Int)
            ]
        ],
      group AllPass
        "Level 1 - Group B (50%)"
        [ testCase 33.33 "Test B1: Pass" (EqMatcher (4 :: Int)) (4 :: Int),
          testCase 33.33 "Test B2: Pass" (EqMatcher (5 :: Int)) (5 :: Int),
          testCase 33.34 "Test B3: Pass" (EqMatcher (6 :: Int)) (6 :: Int)
        ]
    ]

main :: IO ()
main = do
  _ <- setupTerminalWithChcp
  putStrLn "Running example tree:"
  runTestTree exampleTree