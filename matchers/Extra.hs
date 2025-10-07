module Extra where

import Core
import MatcherCombinators

-- Between Matcher
data BetweenMatcher a = BetweenMatcher a a

betweenMatcher low high = (LtMatcher high) `And` (GtMatcher low)

-- Increasing list matcher
data IncreasingList a = IncreasingList

instance (Ord a, Show a) => Matcher (IncreasingList a) [a] where
  matches IncreasingList list = isIncreasingList list
    where
      isIncreasingList [] = True
      isIncreasingList (x : y : xs)
        | x < y = isIncreasingList (y : xs)
        | otherwise = False
      isIncreasingList _ = True
  describe IncreasingList ok
    | ok = " is strictly increasing"
    | otherwise = "is not strictly increasing"
  explainMatch _ list = explainMatchHelper 0 1 list

explainMatcher _ _ [] = "which is strictly increasing"

explainMatchHelper pos1 pos2 (x : y : xs)
  | matches (GtMatcher x) y = explainMatchHelper (pos1 + 1) (pos2 + 1) (y : xs)
  | otherwise = "which is not strictly increasing because the elements at positions " ++ show pos1 ++ " and " ++ show pos2 ++ " do not respect the following " ++ show y ++ " " ++ explainMatch (GtMatcher x) y
explainMatchHelper _ _ _ = " which is strictly increasing "