module TestAssertions where

import Data.List (intercalate)
import Matchers.Core
import Matchers.Combinators

data Test where
  TestPure :: (Matchable m a, Show a) => String -> Bool -> m -> a -> Test

runTest :: Test -> IO Bool
runTest t = do
  (name, expect, got, explanation) <-
    case t of
      TestPure nm expct m x -> do
        let ok = matches m x
            expl = explainMatch m x
        pure (nm ++ "  value=" ++ show x, expct, ok, expl)

  let pass = got == expect
      status = if pass then "PASS" else "FAIL"
  putStrLn $
    intercalate
      " "
      [ name ++ ":",
        status,
        "(expected=" ++ show expect ++ ", got=" ++ show got ++ ")",
        "--",
        explanation
      ]
  pure pass

-- composite and multicomposite

compInt1 :: Composite Int
compInt1 =
  AllOf
    [ One (AnyMatcher (GtMatcher (0 :: Int))),
      NotOf (One (AnyMatcher (EqMatcher (7 :: Int))))
    ]

compIntNested :: Composite Int
compIntNested =
  AnyOf
    [ compInt1,
      AllOf
        [ One (AnyMatcher (EqMatcher (42 :: Int))),
          One (AnyMatcher (GtMatcher (10 :: Int)))
        ]
    ]

multi1 :: MultiComposite
multi1 =
  AllOfM
    [ OneM (AnyMatch (EqMatcher (5 :: Int)) (5 :: Int)),
      OneM (AnyMatch (ContainsMatcher "ell") ("hello" :: String)),
      OneM (AnyMatch compInt1 (3 :: Int))
    ]

multi2 :: MultiComposite
multi2 =
  AllOfM
    [ OneM (AnyMatch (EqMatcher (5 :: Int)) (5 :: Int)),
      OneM (AnyMatch (EqMatcher (999 :: Int)) (0 :: Int))
    ]

multi3 :: MultiComposite
multi3 =
  AnyOfM
    [ OneM (AnyMatch (EqMatcher (999 :: Int)) (0 :: Int)),
      NotOfM (OneM (AnyMatch (EndsWithMatcher "xx") ("hello" :: String)))
    ]

allTests :: [Test]
allTests =
  [ -- simple
    TestPure "EqMatcher: 5 == 5" True (EqMatcher (5 :: Int)) (5 :: Int),
    TestPure "GtMatcher: 0 < 1" True (GtMatcher (0 :: Int)) (1 :: Int),
    TestPure "ContainsMatcher: ell in hello" True (ContainsMatcher "ell") "hello",

    -- compisite
    TestPure "Composite Int: (x>0) AND NOT(x==7) holds for 3" True compInt1 (3 :: Int),
    TestPure "Composite Int: (x>0) AND NOT(x==7) fails for 7" False compInt1 (7 :: Int),
    TestPure "Composite Int (nested): should pass on 42" True compIntNested (42 :: Int),
    TestPure "Composite Int (nested): should fail on -1" False compIntNested ((-1) :: Int),

    TestPure "MultiComposite: all leaves succeed" True multi1 (),
    TestPure "MultiComposite: AllOf fails when one leaf fails" False multi2 (),
    TestPure "MultiComposite: AnyOf passes when one branch succeeds" True multi3 ()
  ]

runAll :: IO ()
runAll = do
  results <- mapM runTest allTests
  let total = length results
      passed = length (filter id results)
      failed = total - passed
  putStrLn $ "\nSummary: " ++ show passed ++ " passed, " ++ show failed ++ " failed (" ++ show total ++ " total)."
  if failed == 0
    then putStrLn "ALL TESTS PASSED"
    else putStrLn "SOME TESTS FAILED"

main :: IO ()
main = runAll
