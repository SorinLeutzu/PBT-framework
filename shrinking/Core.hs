module Shrinking.Core where

import Random.Core (SortedList(..), printableChars, alphaNumChars)
import Config (TupleShrinkStrategy(..), tupleShrinkStrategy)
import Data.List (nub, sort)

class Shrinkable a where
  shrink :: a -> [a]
  shrink _ = []
  extraShrinks :: a -> [a -> [a]]
  extraShrinks _ = []

instance Shrinkable Int where
  shrink = shrinkInt

instance Shrinkable Bool where
  shrink = shrinkBool

instance (Shrinkable a) => Shrinkable [a] where
  shrink list = shrinkList shrinkElem list 1
    where
      shrinkElem :: a -> [a]
      shrinkElem = shrink
  extraShrinks list = buildShrinkAllElFunctions shrinkElem list
    where
      shrinkElem :: a -> [a]
      shrinkElem = shrink

instance Shrinkable Char where
  shrink c = filter (< c) printableChars

instance (Shrinkable a, Shrinkable b) => Shrinkable (a, b) where
  shrink (a, b) = case tupleShrinkStrategy of
    PerDimension  -> [(a', b) | a' <- shrink a] ++ [(a, b') | b' <- shrink b]
    AllDimensions ->
      if null (shrink a) && null (shrink b)
        then []
        else [(shrinkStep a, shrinkStep b)]

instance (Shrinkable a, Shrinkable b, Shrinkable c) => Shrinkable (a, b, c) where
  shrink (a, b, c) = case tupleShrinkStrategy of
    PerDimension  -> [(a', b, c) | a' <- shrink a] ++ [(a, b', c) | b' <- shrink b] ++ [(a, b, c') | c' <- shrink c]
    AllDimensions ->
      if null (shrink a) && null (shrink b) && null (shrink c)
        then []
        else [(shrinkStep a, shrinkStep b, shrinkStep c)]

instance (Shrinkable a, Ord a) => Shrinkable (SortedList a) where
  shrink (SortedList list) = map (SortedList . sort) (shrinkList shrinkElem list 1)
    where
      shrinkElem :: a -> [a]
      shrinkElem = shrink

shrinkPrintableChar :: Char -> [Char]
shrinkPrintableChar c = filter (< c) printableChars

shrinkAlphaNumChar :: Char -> [Char]
shrinkAlphaNumChar c = filter (< c) alphaNumChars

shrinkInt :: Int -> [Int]
shrinkInt 0 = []
shrinkInt x = [0] ++ [abs x | x < 0] ++ takeWhile (\v -> abs v > 0 && abs v < abs x) (tail (iterate (`quot` 2) x))

shrinkBool :: Bool -> [Bool]
shrinkBool True = [False]
shrinkBool False = []

shrinkStep :: (Shrinkable a) => a -> a
shrinkStep x = case shrink x of
  (_ : y : _) -> y
  (y : _)     -> y
  []          -> x

shrinkFirst :: (a -> [a]) -> a -> a
shrinkFirst f x = case f x of
  (y : _) -> y
  [] -> x

-- shrinks lists by removing power of 2 elements
shrinkList :: (a -> [a]) -> [a] -> Int -> [[a]]
shrinkList shrinkingFunction list acc
  | acc > length list = []
  | otherwise =
      let remainingList = drop acc list
          shrunkRemainingList = map (shrinkFirst shrinkingFunction) remainingList
       in remainingList : shrunkRemainingList : shrinkList shrinkingFunction shrunkRemainingList (acc * 2)

-- shrinks list by halving the list until small
reduceSize :: (a -> [a]) -> [a] -> [[a]]
reduceSize shrinkingFunction list =
  let halvedListLen = length list `div` 2
      remainingList = take halvedListLen list
      shrunkRemainingList = map (shrinkFirst shrinkingFunction) remainingList
   in if length list > 2
        then remainingList : shrunkRemainingList : reduceSize shrinkingFunction shrunkRemainingList
        else [list]

dropOneElLeftAndRight :: [a] -> [[a]]
dropOneElLeftAndRight [] = []
dropOneElLeftAndRight [x] = [[]]
dropOneElLeftAndRight list = [init list, tail list]

applyDropOneElLeftAndRight :: [[a]] -> [[a]]
applyDropOneElLeftAndRight = concatMap dropOneElLeftAndRight

orderedSublists :: (Eq a) => [[a]] -> [[a]]
orderedSublists list
  | length (last list) == 1 = list
  | otherwise =
      let subListsOfSameLength = applyDropOneElLeftAndRight list
          shorterSublists = orderedSublists subListsOfSameLength
       in nub (subListsOfSameLength ++ shorterSublists)

shrinkString :: (a -> [a]) -> [a] -> [[a]]
shrinkString _ list
  | length list <= 1 = [[]]
shrinkString shrinkingCharFunction list =
  let firstHalfList = take (length list `div` 2) list
      secondHalfList = drop (length list `div` 2) list
      shrunkList = shrinkingCharFunction (head secondHalfList)
   in shrunkList : firstHalfList : shrinkString shrinkingCharFunction firstHalfList

removeFirst :: [a] -> [[a]]
removeFirst [] = [[]]
removeFirst list@(_ : rest) = list : removeFirst rest

removeLast :: [a] -> [[a]]
removeLast [] = [[]]
removeLast list = list : removeLast (init list)

replaceElAtPos :: [a] -> Int -> a -> [a]
replaceElAtPos list pos newEl = beginning ++ (newEl : end)
  where
    beginning = take pos list
    end = drop (pos + 1) list

shrinkElAtPos :: (a -> [a]) -> Int -> [a] -> [[a]]
shrinkElAtPos shrinkingFunction pos list =
  let shrinkingCandidates = shrinkingFunction (list !! pos)
   in map (\candidate -> replaceElAtPos list pos candidate) shrinkingCandidates

buildShrinkAllElFunctions :: (a -> [a]) -> [a] -> [[a] -> [[a]]]
buildShrinkAllElFunctions shrinkingFunction list =
  let indexes = [0 .. length list - 1]
   in map (\index -> shrinkElAtPos shrinkingFunction index) indexes