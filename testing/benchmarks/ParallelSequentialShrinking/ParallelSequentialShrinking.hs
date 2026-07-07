{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (getArgs)
import Text.Printf (printf)

readScale :: IO Int
readScale = do
  args <- getArgs
  return $ case args of
    (s : _) -> max 1 (read s)
    _       -> 100

shrinkWide :: Int -> [Int]
shrinkWide n
  | n <= 0    = []
  | otherwise = [0] ++ [ n `div` k | k <- [2 .. 40], n `div` k /= 0 ]
                    ++ [ n - k | k <- [1 .. 120], n - k > 0 ]

burnFrom :: Int -> Int -> Int
burnFrom !start 0 = start
burnFrom !start k = burnFrom (start + k) (k - 1)
{-# NOINLINE burnFrom #-}

prop :: Int -> Int -> Int -> Bool
prop burn threshold n = burnFrom n burn `seq` (n < threshold)

firstFail :: (a -> Bool) -> [a] -> Maybe a
firstFail _ [] = Nothing
firstFail p (c : cs)
  | not (p c) = Just c
  | otherwise = firstFail p cs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

shrinkSeq :: (a -> Bool) -> (a -> [a]) -> a -> a
shrinkSeq p shr x
  | p x       = x
  | otherwise = go x (shr x)
  where
    go lf cs = case firstFail p cs of
      Nothing -> lf
      Just f  -> go f (shr f)

shrinkDet :: NFData a => Int -> (a -> Bool) -> (a -> [a]) -> a -> IO a
shrinkDet chunk p shr x
  | p x       = return x
  | otherwise = go x (shr x)
  where
    go lf cs = do
      mvs <- forM (chunksOf chunk cs) $ \c -> do
        mv <- newEmptyMVar
        _  <- forkIO (evaluate (force (firstFail p c)) >>= putMVar mv)
        return mv
      r <- readOrder mvs
      case r of Nothing -> return lf; Just f -> go f (shr f)
    readOrder [] = return Nothing
    readOrder (mv : rest) = do
      v <- takeMVar mv
      case v of Just f -> return (Just f); Nothing -> readOrder rest

shrinkNdet :: NFData a => Int -> (a -> Bool) -> (a -> [a]) -> a -> IO a
shrinkNdet chunk p shr x
  | p x       = return x
  | otherwise = go x (shr x)
  where
    go lf cs = do
      let chs = chunksOf chunk cs
      chan <- newEmptyMVar
      tids <- forM chs $ \c -> forkIO (evaluate (force (firstFail p c)) >>= putMVar chan)
      r <- race tids (length chs) chan
      case r of Nothing -> return lf; Just f -> go f (shr f)
    race _ 0 _ = return Nothing
    race tids k chan = do
      v <- takeMVar chan
      case v of
        Just f  -> mapM_ killThread tids >> return (Just f)
        Nothing -> race tids (k - 1) chan

mkInputs :: Int -> Int -> [Int]
mkInputs count salt = [ 2000000 + salt * 131 + i * 1000 | i <- [1 .. count] ]

benchIO :: (Int -> IO Int) -> IO Double
benchIO f = go 1
  where
    go reps = do
      t0 <- getCurrentTime
      !s <- loop reps 0
      t1 <- getCurrentTime
      let dt = realToFrac (diffUTCTime t1 t0) :: Double
      if dt < 0.05 && reps < 200000000
        then s `seq` go (reps * 4)
        else return (dt / fromIntegral reps)
    loop 0 !acc = return acc
    loop k !acc = do x <- f k; loop (k - 1) (acc + x)

micros :: Double -> Double
micros s = s * 1e6

seqPass :: Int -> Int -> Int -> Int
seqPass burn count salt =
  sum [ shrinkSeq (prop burn x) shrinkWide x | x <- mkInputs count salt ]

detPass :: Int -> Int -> Int -> Int -> IO Int
detPass chunk burn count salt =
  fmap sum (mapM (\x -> shrinkDet chunk (prop burn x) shrinkWide x) (mkInputs count salt))

ndetPass :: Int -> Int -> Int -> Int -> IO Int
ndetPass chunk burn count salt =
  fmap sum (mapM (\x -> shrinkNdet chunk (prop burn x) shrinkWide x) (mkInputs count salt))

chunkSizes :: [Int]
chunkSizes = [1, 5, 10, 20, 50]

runMatcher :: String -> Int -> Int -> IO ()
runMatcher label burn count = do
  putStrLn (label ++ " (" ++ show count ++ " inputs, ~160 candidates each)")
  seqT <- benchIO (\salt -> evaluate (seqPass burn count salt))
  printf "  sequential baseline: %.3f us\n" (micros seqT)
  printf "  %-7s %-12s %-12s %-12s %-12s\n" "chunk" "det(us)" "ndet(us)" "det/seq" "ndet/seq"
  mapM_ (row seqT) chunkSizes
  putStrLn ""
  where
    row seqT chunk = do
      detT  <- benchIO (detPass  chunk burn count)
      ndetT <- benchIO (ndetPass chunk burn count)
      printf "  %-7d %-12.3f %-12.3f %-12.2f %-12.2f\n"
        chunk (micros detT) (micros ndetT) (seqT / detT) (seqT / ndetT)

main :: IO ()
main = do
  scale <- readScale
  let count = max 1 (10 * scale `div` 100)
      burn  = max 1 (50000 * scale `div` 100)
  putStrLn ("Scale " ++ show scale ++ "%  (det/seq and ndet/seq > 1 means parallel is faster)")
  putStrLn ""
  runMatcher "CHEAP matcher (n < threshold, no burn)" 0 count
  runMatcher "EXPENSIVE matcher (burn then n < threshold)" burn count