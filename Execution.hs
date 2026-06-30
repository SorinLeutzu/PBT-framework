module Execution
  ( parallelMap,
    parallelMapN,
    applyShrinkRaw,
    assertThenShrinkRaw,
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
import Config (ShrinkingImpl(..), shrinkingImplementation, chunkSize, numThreads)
import Matchers.Core (Matchable(..))
import TestTree.Types (LogEntry(..))
import PrettyPrinting.Renders (renderLogs, formatResult)

parallelMap :: (NFData b) => (a -> b) -> [a] -> [b]
parallelMap = parallelMapN numThreads

{-# NOINLINE parallelMapN #-}
parallelMapN :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parallelMapN _ _ [] = []
parallelMapN n f xs = unsafePerformIO $ do
  let chunks = chunksOf n xs
  concat <$> mapM (mapChunk f) chunks

mapChunk :: (NFData b) => (a -> b) -> [a] -> IO [b]
mapChunk f xs = do
  mvars <- forM xs $ \x -> do
    mv <- newEmptyMVar
    _ <- forkIO $ evaluate (force (f x)) >>= putMVar mv
    return mv
  mapM takeMVar mvars

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

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

assertThenShrinkRaw :: (Matchable m a, NFData a) => a -> m -> (a -> [a]) -> Writer [LogEntry a] (Maybe a)
assertThenShrinkRaw element matcher getShrinkingCandidates
  | matches matcher element = do
      tell [LogEntry element True]
      return Nothing
  | otherwise = do
      minimal <- applyShrinkRaw element matcher getShrinkingCandidates
      return (Just minimal)

applyShrink :: (Matchable m a, Show a, NFData a) => a -> m -> (a -> [a]) -> (a, [String])
applyShrink el matcher shrinker =
  let (res, logs) = runWriter (applyShrinkRaw el matcher shrinker)
   in (res, renderLogs logs)

assertThenShrink :: (Matchable m a, Show a, NFData a) => a -> m -> (a -> [a]) -> (String, [String])
assertThenShrink el matcher shrinker =
  let (maybeMin, logs) = runWriter (assertThenShrinkRaw el matcher shrinker)
      msg = formatResult maybeMin
   in (msg, renderLogs logs)