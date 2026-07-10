module TestTree.Types where

import Control.DeepSeq (NFData)
import Matchers.Core
import Random.Core qualified as Rand

data LogEntry a = LogEntry
  { leInput :: a,
    lePassed :: Bool
  }
  deriving (Eq, Show)

data GradingPolicy = AllPass | MajorityPass | AnyPass deriving (Show, Eq)

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

instance Matcher (PredMatcher a) where
  describe (PredMatcher _ name) ok =
    if ok then "satisfies " ++ name else "does not satisfy " ++ name

instance Matchable (PredMatcher a) a where
  matches (PredMatcher f _) x = f x

data TestCase where
  UnitCase :: (Matchable m a, Show a) => String -> m -> a -> TestCase
  FuzzCase :: (Rand.Arbitrary a (), Show a, Matchable m a, NFData a) => String -> m -> (a -> [a]) -> Int -> TestCase
  FuzzGenCase :: (Show a, NFData a) => String -> Generator a -> (a -> Bool) -> Int -> TestCase
  FuzzGenMatcherCase :: (Show a, Matchable m a, NFData a) => String -> Generator a -> m -> Int -> TestCase

data TestTree where
  Describe :: GradingPolicy -> String -> [TestTree] -> TestTree
  Test :: Double -> TestCase -> TestTree

instance Show TestTree where
  show = showTree 0
    where
      showTree indent (Describe policy name children) =
        replicate indent ' '
          ++ "Describe [" ++ show policy ++ "] \""
          ++ name
          ++ "\"\n"
          ++ concatMap (showTree (indent + 2)) children
      showTree indent (Test weight tc) =
        replicate indent ' ' ++ "Test (weight=" ++ show weight ++ "%) (" ++ show tc ++ ")\n"

caseName :: TestCase -> String
caseName (UnitCase name _ _) = name
caseName (FuzzCase name _ _ _) = name
caseName (FuzzGenCase name _ _ _) = name
caseName (FuzzGenMatcherCase name _ _ _) = name

instance Show TestCase where
  show (UnitCase name _ _) = "UnitCase \"" ++ name ++ "\""
  show (FuzzCase name _ _ iters) = "FuzzCase \"" ++ name ++ "\" (" ++ show iters ++ " iterations)"
  show (FuzzGenCase name _ _ iters) = "FuzzGenCase \"" ++ name ++ "\" (" ++ show iters ++ " iterations)"
  show (FuzzGenMatcherCase name _ _ iters) = "FuzzGenMatcherCase \"" ++ name ++ "\" (" ++ show iters ++ " iterations)"

data TestResultTree where
  ResultNode :: String -> [TestResultTree] -> TestResultTree
  ResultLeaf :: String -> TestOutcome -> TestResultTree

resultName :: TestResultTree -> String
resultName (ResultLeaf name _) = name
resultName (ResultNode name _) = name