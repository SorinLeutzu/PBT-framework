{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (evaluate)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (getArgs)
import Text.Printf (printf)

import Random.Core (PRNG(..), seedXor, runRandom)
import GrammarFuzzer.GrammarDefinitions
import GrammarFuzzer.Construction (constructParseTreeRnd)
import GrammarFuzzer.Display (evaluateParseTree)
import GrammarFuzzer.Examples (grammarArithmetic, grammarAB, grammarParens)
import GrammarFuzzer.FileSystem (walkFileSystemTree, createFolder)

readScale :: IO Int
readScale = do
  args <- getArgs
  return $ case args of
    (s : _) -> max 1 (read s)
    _       -> 100

genLen :: Grammar' -> Int -> Int
genLen g' salt =
  length (evaluateParseTree (runRandom (PRNG_Xor (seedXor + fromIntegral salt)) (constructParseTreeRnd g')))

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

benchIO :: IO () -> IO Double
benchIO act = go 1
  where
    go reps = do
      t0 <- getCurrentTime
      mapM_ (const act) [1 .. reps]
      t1 <- getCurrentTime
      let dt = realToFrac (diffUTCTime t1 t0) :: Double
      if dt < 0.05 && reps < 1000000
        then go (reps * 4)
        else return (dt / fromIntegral reps)

configs :: Int -> [Int]
configs scale = [ max 1 (k * scale `div` 100) | k <- [2, 4, 6, 8, 10] ]

grammars :: [(String, Grammar)]
grammars = [ ("arithmetic", grammarArithmetic), ("ab", grammarAB), ("parens", grammarParens) ]

part1 :: Int -> IO ()
part1 scale = do
  putStrLn "Part 1: string generation (microseconds per string)"
  printf "%-12s %-8s %-10s %-14s\n" "grammar" "config" "length" "us_per_string"
  mapM_ one [ (name, g, c) | (name, g) <- grammars, c <- configs scale ]
  putStrLn ""
  where
    one (name, g, c) = do
      let g' = Grammar' g c c
          len = genLen g' 1
      t <- benchPure (genLen g')
      printf "%-12s %-8d %-10d %-14.3f\n" name c len (t * 1e6)

fsTree :: Int -> ParseTree
fsTree n = Tree (T "D root")
  (  [ Leaf (T ("f file" ++ show i ++ ".txt content number " ++ show i)) [] | i <- [1 .. n] ]
  ++ [ Tree (T "D docs") [ Leaf (T "f readme.md hello world") [], Leaf (T "E empty") [] ] ]
  )

part2 :: Int -> IO ()
part2 scale = do
  putStrLn "Part 2: file system generation (microseconds)"
  printf "%-10s %-10s %-14s\n" "grammar" "files" "us_total"
  createFolder "baseDir"
  let sized b = max 1 (b * scale `div` 100)
  mapM_ (one sized) [ ("small", 3), ("medium", 6), ("large", 12) ]
  putStrLn ""
  where
    one sized (name, b) = do
      let n = sized b
      t <- benchIO (walkFileSystemTree "baseDir" (fsTree n))
      printf "%-10s %-10d %-14.3f\n" name n (t * 1e6)

main :: IO ()
main = do
  scale <- readScale
  printf "Scale %d%%\n\n" scale
  part1 scale
  part2 scale