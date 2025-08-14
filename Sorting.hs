-- sorting functions
module Sorting where

import Config

-- mergesort
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x : xs) b@(y : ys)
  | x <= y = x : merge xs b
  | otherwise = y : merge ys a

split2 l = splitAt semiLength l
  where
    semiLength = (length l) `div` 2

mergeSort [] = []
mergeSort [el] = [el]
mergeSort l =
  let (left, right) = split2 l
   in merge (mergeSort left) (mergeSort right)

-- quicksort
quickSort [] = []
quickSort [el] = [el]
quickSort (pivot : rest) = quickSort (filter (<= pivot) rest) ++ [pivot] ++ quickSort (filter (> pivot) rest)

sort :: (Ord a) => [a] -> [a]
sort =
  let functionName = sortingFunction
   in case functionName of
        "mergeSort" -> mergeSort
        "quickSort" -> quickSort
        other -> mergeSort -- default sorting function