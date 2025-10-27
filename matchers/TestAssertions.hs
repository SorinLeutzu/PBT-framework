{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestAssertions where

import Core
import Data.List (intercalate)

-- Existential test container: holds any matcher + subject
data Test where
  Test :: (Matcher m a) => String -> Bool -> m -> a -> Test

runTest :: Test -> IO Bool
runTest (Test name expect m x) = do
  let ok = matches m x
      pass = ok == expect
      explanation = explainMatch m x
      status = if pass then "PASS" else "FAIL"
  putStrLn $
    concat
      [ name,
        ": ",
        status,
        " (expected=",
        show expect,
        ", got=",
        show ok,
        ")",
        " -- ",
        explanation
      ]
  return pass

allTests :: [Test]
allTests =
  [ -- EqMatcher
    Test "EqMatcher: success (5 == 5)" True (EqMatcher (5 :: Int)) (5 :: Int),
    Test "EqMatcher: failure (5 /= 4)" False (EqMatcher (5 :: Int)) (4 :: Int),
    -- GtMatcher (lower < higher)
    Test "GtMatcher: success (3 < 5)" True (GtMatcher (3 :: Int)) (5 :: Int),
    Test "GtMatcher: failure (5 < 3)" False (GtMatcher (5 :: Int)) (3 :: Int),
    -- LtMatcher (lower < higher) -- note constructor holds the 'higher' bound
    Test "LtMatcher: success (5 < 10)" True (LtMatcher (10 :: Int)) (5 :: Int),
    Test "LtMatcher: failure (5 < 3)" False (LtMatcher (3 :: Int)) (5 :: Int),
    -- ContainsMatcher (substring)
    Test "ContainsMatcher: success (\"ell\" in \"hello\")" True (ContainsMatcher "ell") "hello",
    Test "ContainsMatcher: failure (\"bye\" not in \"hello\")" False (ContainsMatcher "bye") "hello",
    -- StartsWithMatcher
    Test "StartsWithMatcher: success (\"he\" starts \"hello\")" True (StartsWithMatcher "he") "hello",
    Test "StartsWithMatcher: failure (\"lo\" does not start \"hello\")" False (StartsWithMatcher "lo") "hello",
    -- EndsWithMatcher
    Test "EndsWithMatcher: success (\"lo\" ends \"hello\")" True (EndsWithMatcher "lo") "hello",
    Test "EndsWithMatcher: failure (\"he\" does not end \"hello\")" False (EndsWithMatcher "he") "hello",
    -- Idempotence
    -- id is idempotent; (+1) is not (on integers)
    Test "Idempotence: success (id is idempotent)" True (Idempotence (id :: Int -> Int) 5) (42 :: Int),
    Test "Idempotence: failure ((+1) not idempotent)" False (Idempotence ((+ 1) :: Int -> Int) 3) (0 :: Int),
    -- Invertibility
    Test "Invertibility: success ((+1) and subtract 1)" True (Invertibility ((+ 1) :: Int -> Int) (subtract 1 :: Int -> Int)) (7 :: Int),
    Test "Invertibility: failure (const 0 not invertible)" False (Invertibility (const 0 :: Int -> Int) ((+ 1) :: Int -> Int)) (5 :: Int),
    -- Associativity
    Test "Associativity: success ((+) is associative)" True (Associativity ((+) :: Int -> Int -> Int)) (1, 2, 3),
    Test "Associativity: failure ((-) is not associative)" False (Associativity ((-) :: Int -> Int -> Int)) (3, 2, 1),
    -- Commutativity
    Test "Commutativity: success ((+) is commutative)" True (Commutativity ((+) :: Int -> Int -> Int)) (4, 5),
    Test "Commutativity: failure ((-) is not commutative)" False (Commutativity ((-) :: Int -> Int -> Int)) (4, 5),
    -- Distributivity: times distributes over plus
    Test "Distributivity: success ((* ) distributes over (+))" True (Distributivity ((+) :: Int -> Int -> Int) ((*) :: Int -> Int -> Int)) (2, 3, 4),
    Test "Distributivity: failure ((-) does not distribute over (+))" False (Distributivity ((+) :: Int -> Int -> Int) ((-) :: Int -> Int -> Int)) (5, 3, 2),
    -- ContainsElem
    Test "ContainsElem: success (3 `elem` [1,2,3])" True (ContainsElem (3 :: Int)) [1, 2, 3 :: Int],
    Test "ContainsElem: failure (3 not in [])" False (ContainsElem (3 :: Int)) ([] :: [Int])
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
