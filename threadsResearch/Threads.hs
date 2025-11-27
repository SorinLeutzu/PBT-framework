module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad (mapM_, replicateM, zipWithM_)
import Data.IORef
import Data.List (sort)
import Data.Time.Clock
import System.Environment
import System.Info (os)
import Text.Printf

heavyCompute :: Int -> Int -> Int
heavyCompute iter seed =
  let !resD = loop (fromIntegral seed :: Double) 1
      loop :: Double -> Int -> Double
      loop acc k
        | k > iter = acc
        | otherwise =
            let acc' = acc + sin acc * cos (acc + 0.5) + 0.000001 * fromIntegral (seed * k)
             in loop acc' (k + 1)
   in floor (abs resD)

timeIt :: IO a -> IO (a, Double)
timeIt io = do
  t0 <- getCurrentTime
  r <- io
  t1 <- getCurrentTime
  return (r, realToFrac (diffUTCTime t1 t0) :: Double)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

median :: [Double] -> Double
median xsSorted =
  let n = length xsSorted
   in if odd n
        then xsSorted !! (n `div` 2)
        else
          let a = xsSorted !! (n `div` 2 - 1)
              b = xsSorted !! (n `div` 2)
           in (a + b) / 2

stats :: [Double] -> (Double, Double, Double, Double)
stats xs =
  let s = sort xs
      mn = head s
      mx = last s
      av = mean s
      md = median s
   in (mn, mx, av, md)

-- 1) MVar per thread

runWithMVars :: Int -> Int -> IO (Int, [Double], Double)
runWithMVars n iter = do
  ((sumVals, times), totalTime) <- timeIt $ do
    mvars <- replicateM n newEmptyMVar
    let spawn i mv = forkIO $ do
          t0 <- getCurrentTime
          v <- evaluate (heavyCompute iter i)
          t1 <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
          putMVar mv (v, elapsed)
    zipWithM_ spawn [1 .. n] mvars
    results <- mapM takeMVar mvars
    let vals = map fst results
        tms = map snd results
    return (sum vals, tms)
  return (sumVals, times, totalTime)

-- 2) Chan

runWithChan :: Int -> Int -> IO (Int, [Double], Double)
runWithChan n iter = do
  ((sumVals, times), totalTime) <- timeIt $ do
    ch <- newChan
    let spawn i = forkIO $ do
          t0 <- getCurrentTime
          v <- evaluate (heavyCompute iter i)
          t1 <- getCurrentTime
          writeChan ch (v, realToFrac (diffUTCTime t1 t0) :: Double)
    mapM_ spawn [1 .. n]
    results <- replicateM n (readChan ch)
    let vals = map fst results
        tms = map snd results
    return (sum vals, tms)
  return (sumVals, times, totalTime)

-- 3) IORef atomic accumulation

runWithIORefAtomic :: Int -> Int -> IO (Int, [Double], Double)
runWithIORefAtomic n iter = do
  ((final, times), totalTime) <- timeIt $ do
    acc <- newIORef (0 :: Int)
    ch <- newChan
    let spawn i = forkIO $ do
          t0 <- getCurrentTime
          v <- evaluate (heavyCompute iter i)
          atomicModifyIORef' acc (\old -> (old + v, ()))
          t1 <- getCurrentTime
          writeChan ch (v, realToFrac (diffUTCTime t1 t0) :: Double)
    mapM_ spawn [1 .. n]
    results <- replicateM n (readChan ch)
    final' <- readIORef acc
    return (final', map snd results)
  return (final, times, totalTime)

-- 4) STM (TVar) accumulation

runWithSTM :: Int -> Int -> IO (Int, [Double], Double)
runWithSTM n iter = do
  ((final, times), totalTime) <- timeIt $ do
    tSum <- atomically $ newTVar (0 :: Int)
    ch <- newChan
    let spawn i = forkIO $ do
          t0 <- getCurrentTime
          v <- evaluate (heavyCompute iter i)
          atomically $ modifyTVar' tSum (+ v)
          t1 <- getCurrentTime
          writeChan ch (v, realToFrac (diffUTCTime t1 t0) :: Double)
    mapM_ spawn [1 .. n]
    results <- replicateM n (readChan ch)
    final' <- atomically $ readTVar tSum
    return (final', map snd results)
  return (final, times, totalTime)

type Method = (String, Int -> Int -> IO (Int, [Double], Double))

availableMethods :: [Method]
availableMethods =
  [ ("MVar per thread", \n iter -> runWithMVars n iter),
    ("Chan", \n iter -> runWithChan n iter),
    ("IORef atomic", \n iter -> runWithIORefAtomic n iter),
    ("STM (TVar)", \n iter -> runWithSTM n iter)
  ]

runAll :: Int -> Int -> IO ()
runAll n iter = do
  putStrLn $ "Benchmark: n = " ++ show n ++ ", iter = " ++ show iter
  let methods = availableMethods
  mapM_ (runAndPrint n iter) methods

runAndPrint :: Int -> Int -> Method -> IO ()
runAndPrint n iter (name, action) = do
  putStrLn $ "\n=== Method: " ++ name ++ " ==="
  (res, times, totalTime) <- action n iter
  printf "Total sum = %d\n" res
  if null times || all (== 0) times
    then printf "Total wall time = %.6f s  (per-task times unavailable or not measured)\n" totalTime
    else do
      let sTimes = sort times
          (mn, mx, av, md) = stats sTimes
      printf "Total wall time = %.6f s\n" totalTime
      printf "Per-task times (sec): min=%.6f max=%.6f avg=%.6f median=%.6f\n" mn mx av md

main :: IO ()
main = runAll 3000 100000

-- Benchmark: n = 3000, iter = 100000

-- === Method: MVar per thread ===
-- Total sum = 22439212480
-- Total wall time = 6.129785 s
-- Per-task times (sec): min=0.013791 max=0.590937 avg=0.074174 median=0.049536

-- === Method: Chan ===
-- Total sum = 22439212480
-- Total wall time = 6.000275 s
-- Per-task times (sec): min=0.011785 max=0.598389 avg=0.070202 median=0.042516

-- === Method: IORef atomic ===
-- Total sum = 22439212480
-- Total wall time = 6.459502 s
-- Per-task times (sec): min=0.018083 max=0.508005 avg=0.064646 median=0.041609

-- === Method: STM (TVar) ===
-- Total sum = 22439212480
-- Total wall time = 6.002979 s
-- Per-task times (sec): min=0.015792 max=0.470846 avg=0.062153 median=0.039442