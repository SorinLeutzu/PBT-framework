module TestTree where

import Control.Monad.Writer (runWriter)
import Data.List (intercalate)
import Demo qualified
import Matchers.Core
import Random qualified as Rand

data TestOutcome
  = TestPass String
  | TestFail String
  deriving (Eq, Show)

data Generator a = Generator
  { genA :: Rand.Gen a,
    shrinkA :: a -> [a]
  }

data PredMatcher a = PredMatcher
  { predFn :: a -> Bool,
    predName :: String
  }

instance Matcher (PredMatcher a) a where
  matches (PredMatcher f _) x = f x
  describe (PredMatcher _ name) ok =
    if ok then "satisfies " ++ name else "does not satisfy " ++ name

data TestCase where
  UnitCase :: (Matcher m a, Show a) => String -> m -> a -> TestCase
  FuzzCase :: (Rand.Arbitrary a, Show a, Matcher m a) => String -> m -> (a -> [a]) -> Int -> TestCase
  FuzzGenCase :: (Show a) => String -> Generator a -> (a -> Bool) -> Int -> TestCase

data TestTree where
  Describe :: String -> [TestTree] -> TestTree
  Test :: Double -> TestCase -> TestTree

data TestTreeWithId where
  DescribeWithId :: Int -> String -> [TestTreeWithId] -> TestTreeWithId
  TestWithId :: Int -> Double -> TestCase -> TestTreeWithId

instance Show TestTree where
  show = showTree 0
    where
      showTree indent (Describe name children) =
        indentStr indent
          ++ "Describe \""
          ++ name
          ++ "\"\n"
          ++ concatMap (showTree (indent + 2)) children
      showTree indent (Test weight tc) =
        indentStr indent ++ "Test (weight=" ++ show weight ++ "%) (" ++ show tc ++ ")\n"
      indentStr n = replicate n ' '

instance Show TestTreeWithId where
  show = showTreeWithId 0
    where
      showTreeWithId indent (DescribeWithId id name children) =
        indentStr indent
          ++ "DescribeWithId (id="
          ++ show id
          ++ ") \""
          ++ name
          ++ "\"\n"
          ++ concatMap (showTreeWithId (indent + 2)) children
      showTreeWithId indent (TestWithId id weight tc) =
        indentStr indent ++ "TestWithId (id=" ++ show id ++ ", weight=" ++ show weight ++ "%) (" ++ show tc ++ ")\n"
      indentStr n = replicate n ' '

group :: String -> [TestTree] -> TestTree
group = Describe

testCase :: (Matcher m a, Show a) => Double -> String -> m -> a -> TestTree
testCase weight label m x = Test weight (UnitCase label m x)

fuzz :: forall a m. (Rand.Arbitrary a, Show a, Matcher m a) => Double -> String -> m -> (a -> [a]) -> Int -> TestTree
fuzz weight label matcher shrinker n = Test weight (FuzzCase label matcher shrinker n)

fromArbitrary :: (Rand.Arbitrary a) => Generator a
fromArbitrary = Generator {genA = Rand.arbitrary, shrinkA = Rand.shrink}

fuzzGen :: forall a. (Show a) => Double -> String -> Generator a -> (a -> Bool) -> Int -> TestTree
fuzzGen weight label generator prop n = Test weight (FuzzGenCase label generator prop n)

caseName :: TestCase -> String
caseName (UnitCase name _ _) = name
caseName (FuzzCase name _ _ _) = name
caseName (FuzzGenCase name _ _ _) = name

instance Show TestCase where
  show (UnitCase name _ _) = "UnitCase \"" ++ name ++ "\""
  show (FuzzCase name _ _ iters) = "FuzzCase \"" ++ name ++ "\" (" ++ show iters ++ " iterations)"
  show (FuzzGenCase name _ _ iters) = "FuzzGenCase \"" ++ name ++ "\" (" ++ show iters ++ " iterations)"

runTestCase :: TestCase -> IO TestOutcome
runTestCase (UnitCase _ m x) = do
  let ok = matches m x
      expl = explainMatch m x
  pure $ if ok then TestPass expl else TestFail expl
runTestCase (FuzzCase _ matcher shrinker iters) = do
  let loop :: Int -> Rand.PRNG -> IO TestOutcome
      loop i prng =
        if i == iters
          then pure $ TestPass ("All " ++ show iters ++ " cases passed")
          else do
            let (x, prng') = Rand.execRandom prng Rand.arbitrary
            if matches matcher x
              then loop (i + 1) prng'
              else do
                let (maybeMin, logs) = runWriter (Demo.assertThenShrinkRaw x matcher shrinker)
                    rendered = Demo.renderLogs logs
                    header = case maybeMin of
                      Nothing -> "Initial failure reported but matched after shrinking (unexpected)"
                      Just minimal -> "Found counterexample; minimal = " ++ show minimal
                    info = "Fuzz failure at iteration " ++ show i ++ " of " ++ show iters ++ " with input " ++ show x
                    body = unlines (map ("  " ++) rendered)
                pure $ TestFail (header ++ "\n" ++ info ++ "\n" ++ body)
  loop 0 (Rand.PRNG_Xor Rand.seedXor)
runTestCase (FuzzGenCase _ generator prop iters) = do
  let pm = PredMatcher {predFn = prop, predName = "property"}
      loop :: Int -> Rand.PRNG -> IO TestOutcome
      loop i prng =
        if i == iters
          then pure $ TestPass ("All " ++ show iters ++ " cases passed")
          else do
            let (x, prng') = Rand.execRandom prng (genA generator)
            if matches pm x
              then loop (i + 1) prng'
              else do
                let (maybeMin, logs) = runWriter (Demo.assertThenShrinkRaw x pm (shrinkA generator))
                    rendered = Demo.renderLogs logs
                    header = case maybeMin of
                      Nothing -> "Initial failure reported but matched after shrinking (unexpected)"
                      Just minimal -> "Found counterexample; minimal = " ++ show minimal
                    info = "Fuzz failure at iteration " ++ show i ++ " of " ++ show iters ++ " with input " ++ show x
                    body = unlines (map ("  " ++) rendered)
                pure $ TestFail (header ++ "\n" ++ info ++ "\n" ++ body)
  loop 0 (Rand.PRNG_Xor Rand.seedXor)

data TestResultTree where
  ResultNode :: String -> [TestResultTree] -> TestResultTree
  ResultLeaf :: String -> TestOutcome -> TestResultTree

executeTestTree :: TestTree -> IO TestResultTree
executeTestTree (Describe name children) = do
  rs <- mapM executeTestTree children
  pure (ResultNode name rs)
executeTestTree (Test _ tc) = do
  outcome <- runTestCase tc
  pure (ResultLeaf (caseName tc) outcome)

renderResults :: TestResultTree -> IO (Int, Int)
renderResults = go 0
  where
    go indent (ResultNode name children) = do
      putStrLn (indentStr indent ++ "Suite: " ++ name)
      pairs <- mapM (go (indent + 2)) children
      let (p, f) = foldl (\(p1, f1) (p2, f2) -> (p1 + p2, f1 + f2)) (0, 0) pairs
      pure (p, f)
    go indent (ResultLeaf name outcome) = do
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

runTestTree :: TestTree -> IO ()
runTestTree tt = do
  results <- executeTestTree tt
  (passed, failed) <- renderResults results
  let total = passed + failed
  putStrLn ""
  putStrLn ("Summary: " ++ show passed ++ " passed, " ++ show failed ++ " failed (" ++ show total ++ " total)")

exampleTree :: TestTree
exampleTree =
  group
    "Root"
    [ group
        "Strings"
        [ testCase 25.0 "Contains \"ell\" in \"hello\"" (ContainsMatcher "ell") "hello",
          testCase 25.0 "Starts with \"he\"" (StartsWithMatcher "he") "hello",
          testCase 25.0 "Ends with \"lo\"" (EndsWithMatcher "lo") "hello",
          testCase 25.0 "Equality 5 == 5" (EqMatcher (5 :: Int)) (5 :: Int)
        ],
      group
        "Arithmetic properties (Int)"
        [ fuzz 33.33 "(+): associativity on triples" (Associativity ((+) :: Int -> Int -> Int)) Rand.shrink 200,
          fuzz 33.33 "(+): commutativity on pairs" (Commutativity ((+) :: Int -> Int -> Int)) Rand.shrink 200,
          fuzz
            33.34
            "(* distributes over +)"
            (Distributivity ((+) :: Int -> Int -> Int) ((*) :: Int -> Int -> Int))
            Rand.shrink
            200
        ],
      group
        "Simple matcher with fuzzed single Int"
        [ fuzz 100.0 "gt 0 holds for positive Ints (may fail)" (gt (0 :: Int)) Rand.shrink 100
        ]
    ]

exampleTreeFailing :: TestTree
exampleTreeFailing =
  group
    "Root (Failing examples)"
    [ group
        "Int properties (expected to fail)"
        [ fuzz 50.0 "equals 5 for random Ints (expected to fail)" (EqMatcher (5 :: Int)) Rand.shrink 100,
          fuzz 50.0 "(-) associative on triples (expected to fail)" (Associativity ((-) :: Int -> Int -> Int)) Rand.shrink 100
        ],
      group
        "String properties (expected to fail)"
        [ fuzz 50.0 "contains \"xyz\" (expected to fail)" (ContainsMatcher "xyz") Rand.shrink 100,
          fuzz 50.0 "starts with \"hello\" (expected to fail)" (StartsWithMatcher "hello") Rand.shrink 100
        ],
      group
        "Unit tests (failing)"
        [ testCase 100.0 "Equality 5 == 42 (expected to fail)" (EqMatcher (5 :: Int)) (42 :: Int)
        ]
    ]

exampleTreeNested :: TestTree
exampleTreeNested =
  group
    "Root"
    [ group
        "Level 1 - Group A (50%)"
        [ testCase 50.0 "Test A1: Simple pass" (EqMatcher (1 :: Int)) (1 :: Int),
          group
            "Level 2 - Subgroup A1 (50%)"
            [ testCase 50.0 "Test A1.1: Nested pass" (EqMatcher (2 :: Int)) (2 :: Int),
              testCase 50.0 "Test A1.2: Nested pass" (EqMatcher (3 :: Int)) (3 :: Int)
            ]
        ],
      group
        "Level 1 - Group B (50%)"
        [ testCase 33.33 "Test B1: Pass" (EqMatcher (4 :: Int)) (4 :: Int),
          testCase 33.33 "Test B2: Pass" (EqMatcher (5 :: Int)) (5 :: Int),
          testCase 33.34 "Test B3: Pass" (EqMatcher (6 :: Int)) (6 :: Int)
        ]
    ]

main :: IO ()
main = do
  putStrLn "Example tree:"
  putStrLn (show exampleTree)
  putStrLn "Example tree failing:"
  putStrLn (show exampleTreeFailing)
  putStrLn "Running example tree:"
  runTestTree exampleTree
  putStrLn "Running example tree failing:"
  runTestTree exampleTreeFailing
