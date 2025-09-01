module Demo where

import Assertions (Matcher (..), eq)
import Control.Monad.Writer (Writer, runWriter, tell)
import Random (shrinkIntAgg, shrinkIntStd)

applyShrink :: (Matcher m a, Show a) => a -> m -> (a -> [a]) -> Writer [String] a
applyShrink element matcher getShrinkingCandidates = do
  let initialPassed = matches matcher element
  tell ["Tried input " ++ show element ++ " - Assertion " ++ (if initialPassed then "passed" else "failed")]
  if initialPassed
    then return element
    else go element (getShrinkingCandidates element)
  where
    go lastFail [] = return lastFail
    go lastFail (c : cs) = do
      let passed = matches matcher c
      tell ["Tried input " ++ show c ++ " - Assertion " ++ (if passed then "passed" else "failed")]
      if passed
        then return lastFail
        else go c cs

assertThenShrink :: (Matcher m a, Show a) => a -> m -> (a -> [a]) -> Writer [String] String
assertThenShrink element matcher getShrinkingCandidates
  | matches matcher element = do
      tell ["Tried input " ++ show element ++ " - Assertion passed"]
      return "Matching succeeded"
  | otherwise = do
      minimal <- applyShrink element matcher getShrinkingCandidates
      return $ "Matching failled. Searching for simplest counter example " ++ show minimal

t1 :: (Int, [String])
t1 = runWriter (applyShrink (64 :: Int) (eq (8 :: Int)) shrinkIntStd)

-- ghci> t1
-- (16,["Tried input 64 - Assertion failed","Tried input 64 - Assertion failed","Tried input 32 - Assertion failed","Tried input 16 - Assertion failed","Tried input 8 - Assertion passed"])
t2 :: (String, [String])
t2 = runWriter (assertThenShrink (40940 :: Int) (eq (144 :: Int)) shrinkIntAgg)

-- ghci> t2
-- ("Matching failled. Searching for simplest counter example 1",["Tried input 40940 - Assertion failed","Tried input 40940 - Assertion failed","Tried input 16 - Assertion failed","Tried input 4 - Assertion failed","Tried input 2 - Assertion failed","Tried input 1 - Assertion failed"])