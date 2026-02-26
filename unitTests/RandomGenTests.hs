{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RandomGenTests where

import Data.Word qualified as W
import Matchers.Combinators
import Matchers.Core
import Random
import TestAssertions

testNextWord64Different :: Test
testNextWord64Different =
  let seed = PRNG_Xor seedXor
      (w1, prng1) = nextPRNG seed
      (w2, _) = nextPRNG prng1
   in Test "nextWord64: produces different values" True (Not (EqMatcher w1)) w2

testNextWord64Range :: Test
testNextWord64Range =
  let seed = PRNG_Xor seedXor
      (w, _) = nextPRNG seed
   in Test "nextWord64: produces valid Word64" True (And (GtMatcher (0 :: W.Word64)) (LtMatcher (maxBound :: W.Word64))) w

testNextDoubleRange :: Test
testNextDoubleRange =
  let seed = PRNG_Xor seedXor
      d = runRandom seed nextDouble
   in Test "nextDouble: produces value in [0, 1)" True (And (GtMatcher (-0.0001 :: Double)) (LtMatcher (1.0 :: Double))) d

testNextDoubleNonNegative :: Test
testNextDoubleNonNegative =
  let seed = PRNG_Xor seedXor
      d = runRandom seed nextDouble
   in Test "nextDouble: produces non-negative value" True (GtMatcher (-0.0001 :: Double)) d

testNextDoubleLessThanOne :: Test
testNextDoubleLessThanOne =
  let seed = PRNG_Xor seedXor
      d = runRandom seed nextDouble
   in Test "nextDouble: produces value less than 1" True (LtMatcher (1.0 :: Double)) d

testNextDoubleRangeBounds :: Test
testNextDoubleRangeBounds =
  let seed = PRNG_Xor seedXor
      lo = 5.0
      hi = 10.0
      d = runRandom seed (nextDoubleRange lo hi)
   in Test "nextDoubleRange: produces value in [5.0, 10.0)" True (And (GtMatcher (4.99 :: Double)) (LtMatcher (10.0 :: Double))) d

testNextIntDifferent :: Test
testNextIntDifferent =
  let seed = PRNG_Xor seedXor
      i1 = runRandom seed nextInt
      prng1 = evalRandom seed nextInt
      i2 = runRandom prng1 nextInt
   in Test "nextInt: produces different values" True (Not (EqMatcher i1)) i2

testNextIntRangeBounds :: Test
testNextIntRangeBounds =
  let seed = PRNG_Xor seedXor
      lo = 10
      hi = 20
      i = runRandom seed (nextIntRange lo hi)
   in Test "nextIntRange: produces value in [10, 20)" True (And (GtMatcher (9 :: Int)) (LtMatcher (20 :: Int))) i

testNextIntRangeEqualBounds :: Test
testNextIntRangeEqualBounds =
  let seed = PRNG_Xor seedXor
      val = 42
      i = runRandom seed (nextIntRange val val)
   in Test "nextIntRange: equal bounds returns lower bound" True (EqMatcher val) i

testNextIntRangeInvalidBounds :: Test
testNextIntRangeInvalidBounds =
  let seed = PRNG_Xor seedXor
      lo = 20
      hi = 10
      i = runRandom seed (nextIntRange lo hi)
   in Test "nextIntRange: hi < lo returns lo" True (EqMatcher lo) i

testNextBoolIsBool :: Test
testNextBoolIsBool =
  let seed = PRNG_Xor seedXor
      b = runRandom seed nextBool
   in Test "nextBool: produces boolean value" True (Or (EqMatcher True) (EqMatcher False)) b

testNextBoolVariety :: Test
testNextBoolVariety =
  let seed = PRNG_Xor seedXor
      results = take 100 $ iterate (\prng -> evalRandom prng nextBool) seed
      bools = map (\prng -> runRandom prng nextBool) results
      hasTrue = True `elem` bools
      hasFalse = False `elem` bools
   in Test "nextBool: produces both True and False" True (EqMatcher True) (hasTrue && hasFalse)

testPRNGTypesDifferent :: Test
testPRNGTypesDifferent =
  let seedX = PRNG_Xor seedXor
      seedM = PRNG_Mersenne seedMersenne
      (w1, _) = nextPRNG seedX
      (w2, _) = nextPRNG seedM
   in Test "PRNG: different types produce different values" True (Not (EqMatcher w1)) w2

testPRNGDeterministic :: Test
testPRNGDeterministic =
  let seed = PRNG_Xor seedXor
      (w1, _) = nextPRNG seed
      (w2, _) = nextPRNG seed
   in Test "PRNG: same seed produces same value" True (EqMatcher w1) w2

testNextPositiveIntNonNegative :: Test
testNextPositiveIntNonNegative =
  let seed = PRNG_Xor seedXor
      i = runRandom seed nextPositiveInt
   in Test "nextPositiveInt: produces non-negative value" True (GtMatcher (-1 :: Int)) i

testRandomSampleNonEmpty :: Test
testRandomSampleNonEmpty =
  let seed = PRNG_Xor seedXor
      list = [1, 2, 3, 4, 5]
      result = runRandom seed (randomSample 0 list)
   in Test "randomSample: returns element from list" True (ContainsElem result) list

testRandomSampleEmpty :: Test
testRandomSampleEmpty =
  let seed = PRNG_Xor seedXor
      def = 42
      result = runRandom seed (randomSample def [])
   in Test "randomSample: empty list returns default" True (EqMatcher def) result

allRandomGenTests :: [Test]
allRandomGenTests =
  [ testNextWord64Different,
    testNextWord64Range,
    testNextDoubleRange,
    testNextDoubleNonNegative,
    testNextDoubleLessThanOne,
    testNextDoubleRangeBounds,
    testNextIntDifferent,
    testNextIntRangeBounds,
    testNextIntRangeEqualBounds,
    testNextIntRangeInvalidBounds,
    testNextBoolIsBool,
    testNextBoolVariety,
    testPRNGTypesDifferent,
    testPRNGDeterministic,
    testNextPositiveIntNonNegative,
    testRandomSampleNonEmpty,
    testRandomSampleEmpty
  ]

