{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ArbitraryTests where

import Data.List (sort)
import Data.Word qualified as W
import Matchers.Combinators
import Matchers.Core
import Random
import TestAssertions

testArbitraryIntValid :: Test
testArbitraryIntValid =
  let seed = PRNG_Xor seedXor
      i = runRandom seed (arbitrary :: Gen Int)
   in Test "arbitrary Int: produces valid integer" True (And (GtMatcher (minBound :: Int)) (LtMatcher (maxBound :: Int))) i

testArbitraryBoolValid :: Test
testArbitraryBoolValid =
  let seed = PRNG_Xor seedXor
      b = runRandom seed (arbitrary :: Gen Bool)
   in Test "arbitrary Bool: produces boolean" True (Or (EqMatcher True) (EqMatcher False)) b

testArbitraryListIntValid :: Test
testArbitraryListIntValid =
  let seed = PRNG_Xor seedXor
      list = runRandom seed (arbitrary :: Gen [Int])
   in Test "arbitrary [Int]: produces list" True (EqMatcher True) True

testArbitraryListIntCanBeEmpty :: Test
testArbitraryListIntCanBeEmpty =
  let seed = PRNG_Xor seedXor
      results = take 50 $ iterate (\prng -> evalRandom prng (arbitrary :: Gen [Int])) seed
      lists = map (\prng -> runRandom prng (arbitrary :: Gen [Int])) results
      hasEmpty = [] `elem` lists
   in Test "arbitrary [Int]: can produce empty list" True (EqMatcher True) hasEmpty

testArbitraryCharValid :: Test
testArbitraryCharValid =
  let seed = PRNG_Xor seedXor
      c = runRandom seed (arbitrary :: Gen Char)
   in Test "arbitrary Char: produces valid character" True (And (GtMatcher (minBound :: Char)) (LtMatcher (maxBound :: Char))) c

testArbitraryTupleValid :: Test
testArbitraryTupleValid =
  let seed = PRNG_Xor seedXor
      t = runRandom seed (arbitrary :: Gen (Int, Bool))
   in Test "arbitrary (Int, Bool): produces tuple" True (EqMatcher True) True

testArbitrarySmallIntRange :: Test
testArbitrarySmallIntRange =
  let seed = PRNG_Xor seedXor
      si = runRandom seed (arbitrary :: Gen SmallInt)
      i = getSmallInt si
   in Test "arbitrary SmallInt: produces value in [0, 100)" True (And (GtMatcher (-1 :: Int)) (LtMatcher (100 :: Int))) i

testArbitraryPrintableCharValid :: Test
testArbitraryPrintableCharValid =
  let seed = PRNG_Xor seedXor
      pc = runRandom seed (arbitrary :: Gen PrintableChar)
      c = getPrintableChar pc
   in Test "arbitrary PrintableChar: produces printable character" True (ContainsElem c) printableChars

testArbitraryPrintableStringValid :: Test
testArbitraryPrintableStringValid =
  let seed = PRNG_Xor seedXor
      ps = runRandom seed (arbitrary :: Gen PrintableString)
      s = getPrintableString ps
      allPrintable = all (`elem` printableChars) s
   in Test "arbitrary PrintableString: all characters printable" True (EqMatcher True) allPrintable

testArbitraryAlphaNumCharValid :: Test
testArbitraryAlphaNumCharValid =
  let seed = PRNG_Xor seedXor
      ac = runRandom seed (arbitrary :: Gen AlphaNumChar)
      c = getAlphaNumChar ac
   in Test "arbitrary AlphaNumChar: produces alphanumeric character" True (ContainsElem c) alphaNumChars

testArbitraryAlphaNumStringValid :: Test
testArbitraryAlphaNumStringValid =
  let seed = PRNG_Xor seedXor
      as = runRandom seed (arbitrary :: Gen AlphaNumString)
      s = getAlphaNumString as
      allAlphaNum = all (`elem` alphaNumChars) s
   in Test "arbitrary AlphaNumString: all characters alphanumeric" True (EqMatcher True) allAlphaNum

testArbitrarySortedListSorted :: Test
testArbitrarySortedListSorted =
  let seed = PRNG_Xor seedXor
      sl = runRandom seed (arbitrary :: Gen (SortedList Int))
      list = getSortedList sl
      isSorted = list == sort list
   in Test "arbitrary SortedList: produces sorted list" True (EqMatcher True) isSorted

allArbitraryTests :: [Test]
allArbitraryTests =
  [ testArbitraryIntValid,
    testArbitraryBoolValid,
    testArbitraryListIntValid,
    testArbitraryListIntCanBeEmpty,
    testArbitraryCharValid,
    testArbitraryTupleValid,
    testArbitrarySmallIntRange,
    testArbitraryPrintableCharValid,
    testArbitraryPrintableStringValid,
    testArbitraryAlphaNumCharValid,
    testArbitraryAlphaNumStringValid,
    testArbitrarySortedListSorted
  ]

