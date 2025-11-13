module Demo
  ( LogEntry (..),
    applyShrinkRaw,
    assertThenShrinkRaw,
    renderLog,
    renderLogs,
    formatResult,
    applyShrink,
    assertThenShrink,
  )
where

import Control.Monad (replicateM, when)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List (take)
import Matchers.Core (Matcher (..), eq, gt)
import Random qualified as Rand

data LogEntry a = LogEntry
  { leInput :: a,
    lePassed :: Bool
  }
  deriving (Eq, Show)

applyShrinkRaw :: (Matcher m a) => a -> m -> (a -> [a]) -> Writer [LogEntry a] a
applyShrinkRaw element matcher getShrinkingCandidates = do
  let initialPassed = matches matcher element
  tell [LogEntry element initialPassed]
  if initialPassed
    then return element
    else go element (getShrinkingCandidates element)
  where
    go lastFail [] = return lastFail
    go lastFail (c : cs) = do
      let passed = matches matcher c
      tell [LogEntry c passed]
      if passed
        then return lastFail
        else go c cs

assertThenShrinkRaw :: (Matcher m a) => a -> m -> (a -> [a]) -> Writer [LogEntry a] (Maybe a)
assertThenShrinkRaw element matcher getShrinkingCandidates
  | matches matcher element = do
      tell [LogEntry element True]
      return Nothing
  | otherwise = do
      minimal <- applyShrinkRaw element matcher getShrinkingCandidates
      return (Just minimal)

renderLog :: (Show a) => LogEntry a -> String
renderLog (LogEntry x passed) =
  "Tried input " ++ show x ++ " - Assertion " ++ (if passed then "passed" else "failed")

renderLogs :: (Show a) => [LogEntry a] -> [String]
renderLogs = map renderLog

formatResult :: (Show a) => Maybe a -> String
formatResult Nothing = "Matching succeeded"
formatResult (Just minimal) = "Matching failled. Searching for simplest counter example " ++ show minimal

applyShrink :: (Matcher m a, Show a) => a -> m -> (a -> [a]) -> (a, [String])
applyShrink el matcher shrinker =
  let (res, logs) = runWriter (applyShrinkRaw el matcher shrinker)
   in (res, renderLogs logs)

assertThenShrink :: (Matcher m a, Show a) => a -> m -> (a -> [a]) -> (String, [String])
assertThenShrink el matcher shrinker =
  let (maybeMin, logs) = runWriter (assertThenShrinkRaw el matcher shrinker)
      msg = formatResult maybeMin
   in (msg, renderLogs logs)

t1 :: (Int, [String])
t1 = applyShrink (64 :: Int) (eq (8 :: Int)) Rand.shrinkIntStd

t2 :: (String, [String])
t2 = assertThenShrink (40940 :: Int) (eq (144 :: Int)) Rand.shrinkIntAgg