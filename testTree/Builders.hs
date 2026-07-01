module TestTree.Builders where

import Control.DeepSeq (NFData)
import Matchers.Core
import Random.Core qualified as Rand
import Shrinking.Core qualified as Shrink
import TestTree.Types

group :: GradingPolicy -> String -> [TestTree] -> TestTree
group = Describe

testCase :: (Matchable m a, Show a) => Double -> String -> m -> a -> TestTree
testCase weight label m x = Test weight (UnitCase label m x)

fuzz :: forall a m. (Rand.Arbitrary a (), Show a, Matchable m a, NFData a) => Double -> String -> m -> (a -> [a]) -> Int -> TestTree
fuzz weight label matcher shrinker n = Test weight (FuzzCase label matcher shrinker n)

fromArbitrary :: (Rand.Arbitrary a (), Shrink.Shrinkable a) => Generator a
fromArbitrary = Generator {genA = Rand.arbitrary (), shrinkA = Shrink.shrink}

fuzzGen :: forall a. (Show a, NFData a) => Double -> String -> Generator a -> (a -> Bool) -> Int -> TestTree
fuzzGen weight label generator prop n = Test weight (FuzzGenCase label generator prop n)