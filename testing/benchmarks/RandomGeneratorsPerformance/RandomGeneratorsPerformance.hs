{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (evaluate)
import Data.Word qualified as W
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (getArgs)
import Text.Printf (printf)

import Random.PRNG (nextXor, nextMersenne, nextpcg64)
import Random.Core (seedXor, seedMersenne, seedPcg)

readScale :: IO Int
readScale = do
  args <- getArgs
  return $ case args of
    (s : _) -> max 1 (read s)
    _       -> 100

type Step = W.Word64 -> (W.Word64, W.Word64)

xorStep, merStep, pcgStep :: Step
xorStep s = let v = nextXor s in (v, v)
merStep s = let v = nextMersenne s in (v, v)
pcgStep s = nextpcg64 s

genChecksum :: Step -> W.Word64 -> Int -> Int
genChecksum step seed m = go seed m 0
  where
    go _ 0 !acc = acc
    go s k !acc = let (v, s') = step s in go s' (k - 1) (acc + fromIntegral v)

benchPure :: (Int -> Int) -> IO Double
benchPure f = go 1
  where
    go reps = do
      t0 <- getCurrentTime
      !s <- evaluate (loop reps 0)
      t1 <- getCurrentTime
      let dt = realToFrac (diffUTCTime t1 t0) :: Double
      if dt < 0.05 && reps < 200000000
        then s `seq` go (reps * 4)
        else return (dt / fromIntegral reps)
    loop 0 !acc = acc
    loop k !acc = loop (k - 1) (acc + f k)

main :: IO ()
main = do
  scale <- readScale
  let m = max 1 (10000 * scale `div` 100)
  printf "Scale %d%%: time to generate %d values per generator (microseconds)\n\n" scale m
  printf "%-10s %-16s\n" "generator" "us_per_run"
  bench "XorShift" xorStep seedXor m
  bench "Mersenne" merStep seedMersenne m
  bench "PCG"      pcgStep seedPcg m
  where
    bench name step seed m = do
      t <- benchPure (\salt -> genChecksum step (seed + fromIntegral salt) m)
      printf "%-10s %-16.3f\n" name (t * 1e6)