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

import Control.Monad (forM)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Config (ShrinkingImpl(..), shrinkingImplementation, chunkSize)
import Matchers.Core (Matchable (..), Matcher (..), eq, gt)
import Random.Core qualified as Rand
import Shrinking.Core qualified as Shrink
import Defs
import PrettyPrinting.Renders


applyShrinkRaw :: (Matchable m a, NFData a) => a -> m -> (a -> [a]) -> Writer [LogEntry a] a
applyShrinkRaw element matcher getShrinkingCandidates = do
  let initialPassed = matches matcher element
  tell [LogEntry element initialPassed]
  if initialPassed
    then return element
    else go element (getShrinkingCandidates element)
  where
    go lastFail [] = return lastFail
    go lastFail candidates = do
      let (tested, firstFail) = case shrinkingImplementation of
            Sequential               -> shrinkSeq candidates
            DeterministicParallel    -> shrinkParDet candidates
            NonDeterministicParallel -> shrinkParNonDet candidates
      tell (map (\(c, passed) -> LogEntry c passed) tested)
      case firstFail of
        Nothing -> return lastFail
        Just f  -> go f (getShrinkingCandidates f)

    shrinkSeq [] = ([], Nothing)
    shrinkSeq (c:rest) =
      let passed = matches matcher c
      in if not passed
         then ([(c, passed)], Just c)
         else let (more, result) = shrinkSeq rest
              in ((c, passed) : more, result)

    evalChunk [] = ([], Nothing)
    evalChunk (c:rest) =
      let passed = matches matcher c
      in if not passed
         then ([(c, passed)], Just c)
         else let (more, result) = evalChunk rest
              in ((c, passed) : more, result)

    spawnChunkThreads chunks = forM chunks $ \chunk -> do
      mv <- newEmptyMVar
      tid <- forkIO $ evaluate (force (evalChunk chunk)) >>= putMVar mv
      return (tid, mv)

    shrinkParDet candidates = unsafePerformIO $ do
      entries <- spawnChunkThreads (chunksOf chunkSize candidates)
      scanOrdered entries

    scanOrdered [] = return ([], Nothing)
    scanOrdered ((tid, mv) : rest) = do
      (tested, result) <- takeMVar mv
      case result of
        Just f  -> do mapM_ (\(t, _) -> killThread t) rest
                      return (tested, Just f)
        Nothing -> do (moreTested, laterResult) <- scanOrdered rest
                      return (tested ++ moreTested, laterResult)

    shrinkParNonDet candidates = unsafePerformIO $ do
      let chunks = chunksOf chunkSize candidates
      chan <- newEmptyMVar
      tids <- forM chunks $ \chunk ->
        forkIO $ evaluate (force (evalChunk chunk)) >>= putMVar chan
      scanRacing tids (length chunks) chan

    scanRacing _ 0 _ = return ([], Nothing)
    scanRacing tids remaining chan = do
      (tested, result) <- takeMVar chan
      case result of
        Just f  -> do mapM_ killThread tids
                      return (tested, Just f)
        Nothing -> do (more, later) <- scanRacing tids (remaining - 1) chan
                      return (tested ++ more, later)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

assertThenShrinkRaw :: (Matchable m a, NFData a) => a -> m -> (a -> [a]) -> Writer [LogEntry a] (Maybe a)
assertThenShrinkRaw element matcher getShrinkingCandidates
  | matches matcher element = do
      tell [LogEntry element True]
      return Nothing
  | otherwise = do
      minimal <- applyShrinkRaw element matcher getShrinkingCandidates
      return (Just minimal)

-- renderLog :: (Show a) => LogEntry a -> String
-- renderLog (LogEntry x passed) =
--   "Tried input " ++ show x ++ " - Assertion " ++ (if passed then "passed" else "failed")

-- renderLogs :: (Show a) => [LogEntry a] -> [String]
-- renderLogs = map renderLog

-- formatResult :: (Show a) => Maybe a -> String
-- formatResult Nothing = "Matching succeeded"
-- formatResult (Just minimal) = "Matching failed. Searching for simplest counter example " ++ show minimal

applyShrink :: (Matchable m a, Show a, NFData a) => a -> m -> (a -> [a]) -> (a, [String])
applyShrink el matcher shrinker =
  let (res, logs) = runWriter (applyShrinkRaw el matcher shrinker)
   in (res, renderLogs logs)

assertThenShrink :: (Matchable m a, Show a, NFData a) => a -> m -> (a -> [a]) -> (String, [String])
assertThenShrink el matcher shrinker =
  let (maybeMin, logs) = runWriter (assertThenShrinkRaw el matcher shrinker)
      msg = formatResult maybeMin
   in (msg, renderLogs logs)

t1 :: (Int, [String])
t1 = applyShrink (64 :: Int) (eq (8 :: Int)) Shrink.shrinkInt

t2 :: (String, [String])
t2 = assertThenShrink (40940 :: Int) (eq (144 :: Int)) Shrink.shrinkInt