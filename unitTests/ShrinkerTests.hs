{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ShrinkerTests where

import Data.List (sort)
import Matchers.Core
import Random
import TestAssertions

testShrinkIntStdZero :: Test
testShrinkIntStdZero =
  let result = shrinkIntStd 0
   in Test "shrinkIntStd(0): returns empty list" True (EqMatcher []) result

testShrinkIntStdDecreasing :: Test
testShrinkIntStdDecreasing =
  let x = 100
      result = shrinkIntStd x
      isDecreasing = case result of
        [] -> True
        [y] -> y < x
        ys -> and (zipWith (>) ys (tail ys)) && head ys < x
   in Test "shrinkIntStd: produces decreasing values" True (EqMatcher True) isDecreasing

testShrinkIntStdSpecific :: Test
testShrinkIntStdSpecific =
  let result = shrinkIntStd 8
      expected = [4, 2, 1]
   in Test "shrinkIntStd(8): produces [4, 2, 1]" True (EqMatcher expected) result

testShrinkIntStdAllLess :: Test
testShrinkIntStdAllLess =
  let x = 100
      result = shrinkIntStd x
      allLess = all (< x) result
   in Test "shrinkIntStd: all values less than original" True (EqMatcher True) allLess

testShrinkIntStdApproachesZero :: Test
testShrinkIntStdApproachesZero =
  let x = 100
      result = shrinkIntStd x
      hasSmallValues = not (null result) && last result < 10
   in Test "shrinkIntStd: produces small values" True (EqMatcher True) hasSmallValues

testShrinkBoolTrue :: Test
testShrinkBoolTrue =
  let result = shrinkBool True
   in Test "shrinkBool(True): returns [False]" True (EqMatcher [False]) result

testShrinkBoolFalse :: Test
testShrinkBoolFalse =
  let result = shrinkBool False
   in Test "shrinkBool(False): returns empty list" True (EqMatcher []) result

testShrinkListStdEmpty :: Test
testShrinkListStdEmpty =
  let result = shrinkListStd shrinkIntStd [] 1
   in Test "shrinkListStd: empty list returns empty" True (EqMatcher []) result

testShrinkListStdShorter :: Test
testShrinkListStdShorter =
  let list = [100, 200, 300, 400, 500]
      result = shrinkListStd shrinkIntStd list 1
      allShorter = all (\shrunk -> length shrunk < length list) result
   in Test "shrinkListStd: produces shorter lists" True (EqMatcher True) allShorter

testShrinkListStdAccTooLarge :: Test
testShrinkListStdAccTooLarge =
  let list = [1, 2, 3]
      result = shrinkListStd shrinkIntStd list 10
   in Test "shrinkListStd: acc > length returns empty" True (EqMatcher []) result

testShrinkStringEmpty :: Test
testShrinkStringEmpty =
  let result = shrinkString shrinkPrintableChar []
      converted = map (map getPrintableChar) result
   in Test "shrinkString: empty string returns [[]]" True (EqMatcher [[]]) converted

testShrinkStringSingleChar :: Test
testShrinkStringSingleChar =
  let result = shrinkString shrinkPrintableChar [PrintableChar 'a']
      converted = map (map getPrintableChar) result
   in Test "shrinkString: single char returns [[]]" True (EqMatcher [[]]) converted

testShrinkStringShorter :: Test
testShrinkStringShorter =
  let str = [PrintableChar 'a', PrintableChar 'b', PrintableChar 'c', PrintableChar 'd', PrintableChar 'e']
      result = shrinkString shrinkPrintableChar str
      allShorter = all (\shrunk -> length shrunk < length str) result
   in Test "shrinkString: produces shorter strings" True (EqMatcher True) allShorter

testShrinkIntAggZero :: Test
testShrinkIntAggZero =
  let result = shrinkIntAgg 0
   in Test "shrinkIntAgg(0): returns empty list" True (EqMatcher []) result

testShrinkIntAggDecreasing :: Test
testShrinkIntAggDecreasing =
  let x = 100
      result = shrinkIntAgg x
      isDecreasing = case result of
        [] -> True
        [y] -> y < x
        ys -> and (zipWith (>) ys (tail ys)) && head ys < x
   in Test "shrinkIntAgg: produces decreasing values" True (EqMatcher True) isDecreasing

testShrinkIntInstance :: Test
testShrinkIntInstance =
  let x = 8
      result = shrink x :: [Int]
      expected = shrinkIntStd x
   in Test "shrink Int: uses shrinkIntStd" True (EqMatcher expected) result

testShrinkBoolInstance :: Test
testShrinkBoolInstance =
  let b = True
      result = shrink b :: [Bool]
      expected = shrinkBool b
   in Test "shrink Bool: uses shrinkBool" True (EqMatcher expected) result

testShrinkListInstance :: Test
testShrinkListInstance =
  let list = [1, 2, 3, 4, 5]
      result = shrink list :: [[Int]]
      allAreLists = all (const True) result
   in Test "shrink [Int]: produces list of lists" True (EqMatcher True) allAreLists

testShrinkTupleInstance :: Test
testShrinkTupleInstance =
  let t = (8, True)
      result = shrink t :: [(Int, Bool)]
      allAreTuples = all (const True) result
   in Test "shrink (Int, Bool): produces list of tuples" True (EqMatcher True) allAreTuples

testShrinkSmallIntInstance :: Test
testShrinkSmallIntInstance =
  let si = SmallInt 8
      result = shrink si :: [SmallInt]
      expected = map SmallInt (shrinkIntStd 8)
   in Test "shrink SmallInt: uses shrinkIntStd" True (EqMatcher expected) result

testShrinkPrintableCharInstance :: Test
testShrinkPrintableCharInstance =
  let pc = PrintableChar 'x'
      result = shrink pc :: [PrintableChar]
      allPrintable = all (\pc' -> getPrintableChar pc' `elem` printableChars) result
   in Test "shrink PrintableChar: produces printable chars" True (EqMatcher True) allPrintable

allShrinkerTests :: [Test]
allShrinkerTests =
  [ testShrinkIntStdZero,
    testShrinkIntStdDecreasing,
    testShrinkIntStdSpecific,
    testShrinkIntStdAllLess,
    testShrinkIntStdApproachesZero,
    testShrinkBoolTrue,
    testShrinkBoolFalse,
    testShrinkListStdEmpty,
    testShrinkListStdShorter,
    testShrinkListStdAccTooLarge,
    testShrinkStringEmpty,
    testShrinkStringSingleChar,
    testShrinkStringShorter,
    testShrinkIntAggZero,
    testShrinkIntAggDecreasing,
    testShrinkIntInstance,
    testShrinkBoolInstance,
    testShrinkListInstance,
    testShrinkTupleInstance,
    testShrinkSmallIntInstance,
    testShrinkPrintableCharInstance
  ]

