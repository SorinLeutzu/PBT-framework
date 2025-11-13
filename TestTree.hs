module TestTree () where

import Control.Monad.Writer (runWriter)
import Data.List (intercalate)
import Demo qualified
import Matchers.Core
  ( Associativity (..),
    Commutativity (..),
    ContainsMatcher (..),
    Distributivity (..),
    EndsWithMatcher (..),
    EqMatcher (..),
    Matcher (explainMatch, matches),
    StartsWithMatcher (..),
    eq,
    gt,
  )
import Random qualified as Rand

data TestOutcome
  = TestPass String
  | TestFail String
  deriving (Eq, Show)

data TestTree where
  Describe :: String -> [TestTree] -> TestTree
  Unit :: String -> IO TestOutcome -> TestTree

describe :: String -> [TestTree] -> TestTree
describe = Describe

testCase :: (Matcher m a, Show a) => String -> m -> a -> TestTree
testCase label m x =
  Unit label $ do
    let ok = matches m x
        expl = explainMatch m x
    pure $ if ok then TestPass expl else TestFail expl

fuzz :: forall a m. (Rand.Arbitrary a, Show a, Matcher m a) => String -> m -> (a -> [a]) -> Int -> TestTree
fuzz label matcher shrinker n =
  Unit label $ do
    let prng0 = Rand.PRNG_Xor Rand.seedXor

        firstFailure :: Rand.PRNG -> Int -> Maybe a
        firstFailure prng i
          | i >= n = Nothing
          | otherwise =
              let (x :: a, prng') = Rand.execRandom prng (Rand.arbitrary :: Rand.Gen a)
               in if matches matcher x then firstFailure prng' (i + 1) else Just x

    case firstFailure prng0 0 of
      Nothing -> pure $ TestPass ("All " ++ show n ++ " cases passed")
      Just x -> do
        let (maybeMin, logs) = runWriter (Demo.assertThenShrinkRaw x matcher shrinker)
            rendered = Demo.renderLogs logs
            header = case maybeMin of
              Nothing -> "Initial failure reported but matched after shrinking (unexpected)"
              Just minimal -> "Found counterexample; minimal = " ++ show minimal
            body = unlines (map ("  " ++) rendered)
        pure $ TestFail (header ++ "\n" ++ body)

runTestTree :: TestTree -> IO ()
runTestTree tt = do
  let go :: Int -> TestTree -> IO (Int, Int)
      go indent (Describe name children) = do
        putStrLn (indentStr indent ++ "Suite: " ++ name)
        results <- mapM (go (indent + 2)) children
        let (passed, failed) = foldl (\(p1, f1) (p2, f2) -> (p1 + p2, f1 + f2)) (0, 0) results
        pure (passed, failed)
      go indent (Unit name action) = do
        outcome <- action
        case outcome of
          TestPass msg -> do
            putStrLn (indentStr indent ++ "Test: " ++ name ++ " - PASS")
            putStrLn (indentStr (indent + 2) ++ msg)
            pure (1, 0)
          TestFail msg -> do
            putStrLn (indentStr indent ++ "Test: " ++ name ++ " - FAIL")
            putStrLn (indentStr (indent + 2) ++ msg)
            pure (0, 1)

      indentStr n = replicate n ' '

  (passed, failed) <- go 0 tt
  let total = passed + failed
  putStrLn ""
  putStrLn ("Summary: " ++ show passed ++ " passed, " ++ show failed ++ " failed (" ++ show total ++ " total)")

exampleTree :: TestTree
exampleTree =
  describe
    "Root"
    [ describe
        "Strings"
        [ testCase "Contains \"ell\" in \"hello\"" (ContainsMatcher "ell") "hello",
          testCase "Starts with \"he\"" (StartsWithMatcher "he") "hello",
          testCase "Ends with \"lo\"" (EndsWithMatcher "lo") "hello",
          testCase "Equality 5 == 5" (EqMatcher (5 :: Int)) (5 :: Int)
        ],
      describe
        "Arithmetic properties (Int)"
        [ fuzz "(+): associativity on triples" (Associativity ((+) :: Int -> Int -> Int)) Rand.shrink 200,
          fuzz "(+): commutativity on pairs" (Commutativity ((+) :: Int -> Int -> Int)) Rand.shrink 200,
          fuzz
            "(* distributes over +)"
            (Distributivity ((+) :: Int -> Int -> Int) ((*) :: Int -> Int -> Int))
            Rand.shrink
            200
        ],
      describe
        "Simple matcher with fuzzed single Int"
        [ fuzz "gt 0 holds for positive Ints (may fail)" (gt (0 :: Int)) Rand.shrink 100
        ]
    ]

exampleTreeFailing :: TestTree
exampleTreeFailing =
  describe
    "Root (Failing examples)"
    [ describe
        "Int properties (expected to fail)"
        [ fuzz "equals 5 for random Ints (expected to fail)" (EqMatcher (5 :: Int)) Rand.shrink 100,
          fuzz "(-) associative on triples (expected to fail)" (Associativity ((-) :: Int -> Int -> Int)) Rand.shrink 100
        ],
      describe
        "String properties (expected to fail)"
        [ fuzz "contains \"xyz\" (expected to fail)" (ContainsMatcher "xyz") Rand.shrink 100,
          fuzz "starts with \"hello\" (expected to fail)" (StartsWithMatcher "hello") Rand.shrink 100
        ],
      describe
        "Unit tests (failing)"
        [ testCase "Equality 5 == 42 (expected to fail)" (EqMatcher (5 :: Int)) (42 :: Int)
        ]
    ]
