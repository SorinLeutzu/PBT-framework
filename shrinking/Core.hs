module Shrinking.Core where

import Random.Core (SortedList(..), printableChars, alphaNumChars)
import Data.List (nub)

class Shrinkable a where
  shrink :: a -> [a]
  shrink _ = []
  aggShrink :: a -> [a]
  aggShrink _ = []
  extraShrinks :: a -> [a->[a]] -- a list of extra shrinks that may further reduce the counterexample after standard/aggressive shrinking
  extraShrinks _ = []

instance Shrinkable Int where
  shrink = shrinkIntStd

instance Shrinkable Bool where
  shrink = shrinkBool

instance (Shrinkable a) => Shrinkable [a] where
  shrink list = shrinkListStd shrinkElem list 1
    where
      shrinkElem :: a -> [a]
      shrinkElem = shrink
  extraShrinks list = buildShrinkAllElFunctions shrinkElem list
    where
      shrinkElem :: a -> [a]
      shrinkElem = shrink

instance Shrinkable Char where
  shrink x = printableChars

instance (Shrinkable a, Shrinkable b) => Shrinkable (a,b) where
  shrink (left, right) = zip (shrink left) (shrink right)

instance (Shrinkable a,Shrinkable b, Shrinkable c) => Shrinkable (a,b,c) where
  shrink (left, middle, right) = zip3 (shrink left) (shrink middle) (shrink right)

instance (Shrinkable a, Ord a) => Shrinkable (SortedList a) where
  shrink (SortedList list) = map SortedList (shrinkListStd shrinkElem list 1)
    where
      shrinkElem :: a -> [a]
      shrinkElem = shrink

shrinkPrintableChar :: Char -> [Char]
shrinkPrintableChar _ = printableChars

shrinkAlphaNumChar :: Char -> [Char]
shrinkAlphaNumChar _ = alphaNumChars

shrinkIntStd 0 = []
shrinkIntStd x = takeWhile (/= 0) $ iterate (`div` 2) x



shrinkIntAgg 0 = []
shrinkIntAgg x = takeWhile (/= 0) $ iterate (`div` 10) x

-- further shrink an integer by decrementing its value by a constant
decrement :: Int
decrement = 5

shrink1 0 = []
shrink1 x | sgn x == 1 = takeWhile (>0) $ iterate (subtract decrement) x
          | sgn x == -1 = takeWhile (<0) $ iterate (+decrement) x

ex1 = take 4 $ shrink1 100
ex2 = take 3 $ shrink1 (-43)

sgn x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- log2 :: Int -> Int
-- log2 x | x >= 0 && x < 2 = 0
-- log2 x
--   | x >= 2 = exp2 1 0 x
--   | x < 0 = sgn x * exp2 1 0 (x `div` sgn x)

-- exp2 :: Int -> Int -> Int -> Int
-- exp2 acc power upperBound
--   | acc < upperBound = exp2 (acc * 2) (power + 1) upperBound
--   | otherwise = power

-- works by taking the base 2 logarithm consecutively
-- shrinkIntAgg :: Int -> [Int]
-- shrinkIntAgg 0 = []
-- shrinkIntAgg x = takeWhile (/= 0) $ iterate log2 x

shrinkBool :: Bool -> [Bool]
shrinkBool True = [False]
shrinkBool False = []

-- shrinks lists by removing power of 2 elements
shrinkListStd :: (a -> [a]) -> [a] -> Int -> [[a]]
shrinkListStd shrinkingFunction list acc
  | acc > length list = []
  | otherwise =
      let remainingList = drop acc list
          shrinkedRemainingList = map (!! 1) (map shrinkingFunction remainingList)
       in remainingList : shrinkedRemainingList : shrinkListStd shrinkingFunction shrinkedRemainingList (acc * 2)

-- t = take 8 (shrinkListStd shrinkIntStd [200, 300, 400, 500, 600, 700] 1)

-- shrinks list by halving the list until the list is 10 elements or less
-- then generates all combinations and chooses the shortest
-- shrinkListAgg :: (a -> [a]) -> [a] -> [[a]]
-- shrinkListAgg shrinkingFunction list = reduceSize shrinkingFunction list ++ (orderedSublists shrinkingFunction [list])

reduceSize :: (a -> [a]) -> [a] -> [[a]]
reduceSize shrinkingFunction list =
  let halvedListLen = length list `div` 2
      remainingList = take halvedListLen list
      shrinkedRemainingList = map (!! 1) (map shrinkingFunction remainingList)
   in if (length list > 2)
        then remainingList : shrinkedRemainingList : reduceSize shrinkingFunction shrinkedRemainingList
        else [list]

dropOneElLeftAndRight :: [a] -> [[a]]
dropOneElLeftAndRight list = init list : [tail list]

applyDropOneElLeftAndRight :: [[a]] -> [[a]]
applyDropOneElLeftAndRight list = concat (map dropOneElLeftAndRight list)

orderedSublists :: (Eq a) => [[a]] -> [[a]]
orderedSublists list
  | length (last list) == 1 = list
  | otherwise =
      let subListsOfSameLength = applyDropOneElLeftAndRight list
          shorterSublists = orderedSublists subListsOfSameLength
       in nub (subListsOfSameLength ++ shorterSublists)

shrinkString :: (a -> [a]) -> [a] -> [[a]]
shrinkString shrinkingCharFunction list
  | length list <= 1 = [[]]
  | otherwise =
      let firstHalfList = take ((length list) `div` 2) list
          secondHalfList = drop ((length list) `div` 2) list
          shrunkList = head (map shrinkingCharFunction secondHalfList)
       in shrunkList : firstHalfList : shrinkString shrinkingCharFunction firstHalfList

removeFirst :: [a] -> [[a]]
removeFirst list = iterate (tail) list

ex4 = take 4 $ removeFirst [1,2,3,4,5]

removeLast :: [a] -> [[a]]
removeLast list = iterate (init) list

ex5 = take 4 $ removeLast [1,2,3,4,5]

replaceElAtPos :: [a] -> Int -> a -> [a]
replaceElAtPos list pos newEl = beginning ++ (newEl : end) where
  beginning = take pos list
  end = drop (pos+1) list

shrinkElAtPos :: (a->[a]) -> Int -> [a] -> [[a]]
shrinkElAtPos shrinkingFunction pos list = let shrinkingCandidates = shrinkingFunction (list !! pos )
                                              in map (\candidate -> replaceElAtPos list pos candidate) shrinkingCandidates

-- returns a list of functions that take a list and return the shrinking candidates for a given element of that list
buildShrinkAllElFunctions :: (a->[a]) -> [a] -> [[a]->[[a]]]
buildShrinkAllElFunctions shrinkingFunction list = let indexes = [0..(length list) -1]
                                                       in map (\index-> shrinkElAtPos shrinkingFunction index) indexes
