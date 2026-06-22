module Test where

import Matchers.Core
import Tensors

import Data.List (intercalate)

data Test where
  TestPure :: (Matchable m a, Show a) => String -> Bool -> m -> a -> Test

runTest :: Test -> IO Bool
runTest t = do
  (name, expect, got, explanation) <-
    case t of
      TestPure nm expct m x -> do
        let ok = matches m x
            pass = ok == expct
            expl = explainMatch m x
        pure
          ( nm ++ "  value=" ++ show x,
            expct,
            ok,
            expl
          )

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

mkTensor2x2 :: [Int] -> Tensor 2 Int
mkTensor2x2 xs =
  case constructEmptyTensor [2, 2] of
    Just t -> populateTensor t xs
    Nothing -> error "mkTensor2x2: constructEmptyTensor failed"

allTests :: [Test]
allTests =
  [ -- EqMatcher
    TestPure "EqMatcher: 5 == 5" True (EqMatcher (5 :: Int)) (5 :: Int),
    TestPure "EqMatcher: 5 /= 4" False (EqMatcher (5 :: Int)) (4 :: Int),

    -- GtMatcher
    TestPure "GtMatcher: 3 < 5" True (GtMatcher (3 :: Int)) (5 :: Int),
    TestPure "GtMatcher: 5 < 3" False (GtMatcher (5 :: Int)) (3 :: Int),
    TestPure "GtMatcher: 5 < 5 (strict)" False (GtMatcher (5 :: Int)) (5 :: Int),

    -- LtMatcher
    TestPure "LtMatcher: 5 < 10" True (LtMatcher (10 :: Int)) (5 :: Int),
    TestPure "LtMatcher: 5 < 3" False (LtMatcher (3 :: Int)) (5 :: Int),
    TestPure "LtMatcher: 5 < 5 (strict)" False (LtMatcher (5 :: Int)) (5 :: Int),

    -- String matchers
    TestPure "ContainsMatcher: \"ell\" in \"hello\"" True (ContainsMatcher "ell") "hello",
    TestPure "ContainsMatcher: \"bye\" not in \"hello\"" False (ContainsMatcher "bye") "hello",
    TestPure "StartsWithMatcher: \"he\" starts \"hello\"" True (StartsWithMatcher "he") "hello",
    TestPure "StartsWithMatcher: \"lo\" does not start \"hello\"" False (StartsWithMatcher "lo") "hello",
    TestPure "EndsWithMatcher: \"lo\" ends \"hello\"" True (EndsWithMatcher "lo") "hello",
    TestPure "EndsWithMatcher: \"he\" does not end \"hello\"" False (EndsWithMatcher "he") "hello",

    -- ContainsElem
    TestPure "ContainsElem: 3 in [1,2,3]" True (ContainsElem (3 :: Int)) [1, 2, 3 :: Int],
    TestPure "ContainsElem: 3 not in []" False (ContainsElem (3 :: Int)) ([] :: [Int]),

    -- Idempotence
    TestPure "Idempotence: id is idempotent" True (Idempotence (id :: Int -> Int) 5) (42 :: Int),
    TestPure "Idempotence: (+1) not idempotent" False (Idempotence ((+ 1) :: Int -> Int) 3) (0 :: Int),

    -- Invertibility
    TestPure "Invertibility: (+1) and subtract 1" True (Invertibility ((+ 1) :: Int -> Int) (subtract 1 :: Int -> Int)) (7 :: Int),
    TestPure "Invertibility: const 0 not invertible" False (Invertibility (const 0 :: Int -> Int) ((+ 1) :: Int -> Int)) (5 :: Int),

    -- Associativity
    TestPure "Associativity: (+) associative" True (Associativity ((+) :: Int -> Int -> Int)) ((1, 2, 3) :: (Int, Int, Int)),
    TestPure "Associativity: (-) not associative" False (Associativity ((-) :: Int -> Int -> Int)) ((3, 2, 1) :: (Int, Int, Int)),

    -- Commutativity
    TestPure "Commutativity: (+) commutative" True (Commutativity ((+) :: Int -> Int -> Int)) ((4, 5) :: (Int, Int)),
    TestPure "Commutativity: (-) not commutative" False (Commutativity ((-) :: Int -> Int -> Int)) ((4, 5) :: (Int, Int)),

    -- Distributivity
    TestPure "Distributivity: (*) over (+)" True (Distributivity ((+) :: Int -> Int -> Int) ((*) :: Int -> Int -> Int)) ((2, 3, 4) :: (Int, Int, Int)),
    TestPure "Distributivity: (-) over (+) (should fail)" False (Distributivity ((+) :: Int -> Int -> Int) ((-) :: Int -> Int -> Int)) ((5, 3, 2) :: (Int, Int, Int)),

    -- Tensor matchers
    let t22 = mkTensor2x2 [1, 2, 3, 4]
     in TestPure "HasRank: rank 2 tensor" True (HasRank 2) t22,
    let t22 = mkTensor2x2 [1, 2, 3, 4]
     in TestPure "HasRank: not rank 1" False (HasRank 1) t22,
    let t22 = mkTensor2x2 [1, 2, 3, 4]
     in TestPure "HasShape: [2,2]" True (HasShape [2, 2]) t22,
    let t22 = mkTensor2x2 [1, 2, 3, 4]
     in TestPure "HasShape: not [4]" False (HasShape [4]) t22
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

