module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.DeepSeq (deepseq, force)
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

-- 0) Single-threaded

runSingleThreadLazy :: Int -> Int -> IO (Int, Double)
runSingleThreadLazy n iter = do
  (sumVals, totalTime) <- timeIt $ do
    let compute i = heavyCompute iter i
        vals = map compute [1 .. n]
        result = sum vals
    result `deepseq` return result
  return (sumVals, totalTime)

runSingleThreadEager :: Int -> Int -> IO (Int, Double)
runSingleThreadEager n iter = do
  (sumVals, totalTime) <- timeIt $ do
    let compute i = heavyCompute iter i
    vals <- mapM (\i -> evaluate (compute i)) [1 .. n]
    let result = sum vals
    result `deepseq` return result
  return (sumVals, totalTime)

-- 1) MVar per thread

runWithMVarsLazy :: Int -> Int -> IO (Int, Double)
runWithMVarsLazy n iter = do
  (sumVals, totalTime) <- timeIt $ do
    mvars <- replicateM n newEmptyMVar
    let spawn i mv = forkIO $ do
          let v = heavyCompute iter i
          putMVar mv v
    zipWithM_ spawn [1 .. n] mvars
    vals <- mapM takeMVar mvars
    let result = sum vals
    result `deepseq` return result
  return (sumVals, totalTime)

runWithMVarsEager :: Int -> Int -> IO (Int, Double)
runWithMVarsEager n iter = do
  (sumVals, totalTime) <- timeIt $ do
    mvars <- replicateM n newEmptyMVar
    let spawn i mv = forkIO $ do
          v <- evaluate (heavyCompute iter i)
          putMVar mv v
    zipWithM_ spawn [1 .. n] mvars
    vals <- mapM takeMVar mvars
    let result = sum vals
    result `deepseq` return result
  return (sumVals, totalTime)

-- 2) Chan

runWithChanLazy :: Int -> Int -> IO (Int, Double)
runWithChanLazy n iter = do
  (sumVals, totalTime) <- timeIt $ do
    ch <- newChan
    let spawn i = forkIO $ do
          let v = heavyCompute iter i
          writeChan ch v
    mapM_ spawn [1 .. n]
    vals <- replicateM n (readChan ch)
    let result = sum vals
    result `deepseq` return result
  return (sumVals, totalTime)

runWithChanEager :: Int -> Int -> IO (Int, Double)
runWithChanEager n iter = do
  (sumVals, totalTime) <- timeIt $ do
    ch <- newChan
    let spawn i = forkIO $ do
          v <- evaluate (heavyCompute iter i)
          writeChan ch v
    mapM_ spawn [1 .. n]
    vals <- replicateM n (readChan ch)
    let result = sum vals
    result `deepseq` return result
  return (sumVals, totalTime)

-- 3) IORef atomic accumulation

runWithIORefAtomicLazy :: Int -> Int -> IO (Int, Double)
runWithIORefAtomicLazy n iter = do
  (final, totalTime) <- timeIt $ do
    acc <- newIORef (0 :: Int)
    ch <- newChan
    let spawn i = forkIO $ do
          let v = heavyCompute iter i
          atomicModifyIORef' acc (\old -> (old + v, ()))
          writeChan ch v
    mapM_ spawn [1 .. n]
    replicateM n (readChan ch)
    final' <- readIORef acc
    final' `deepseq` return final'
  return (final, totalTime)

runWithIORefAtomicEager :: Int -> Int -> IO (Int, Double)
runWithIORefAtomicEager n iter = do
  (final, totalTime) <- timeIt $ do
    acc <- newIORef (0 :: Int)
    ch <- newChan
    let spawn i = forkIO $ do
          v <- evaluate (heavyCompute iter i)
          atomicModifyIORef' acc (\old -> (old + v, ()))
          writeChan ch v
    mapM_ spawn [1 .. n]
    replicateM n (readChan ch)
    final' <- readIORef acc
    final' `deepseq` return final'
  return (final, totalTime)

-- 4) STM (TVar) accumulation

runWithSTMLazy :: Int -> Int -> IO (Int, Double)
runWithSTMLazy n iter = do
  (final, totalTime) <- timeIt $ do
    tSum <- newTVarIO (0 :: Int)
    ch <- newChan
    let spawn i = forkIO $ do
          let v = heavyCompute iter i
          atomically $ modifyTVar' tSum (+ v)
          writeChan ch v
    mapM_ spawn [1 .. n]
    replicateM n (readChan ch)
    final' <- readTVarIO tSum
    final' `deepseq` return final'
  return (final, totalTime)

runWithSTMEager :: Int -> Int -> IO (Int, Double)
runWithSTMEager n iter = do
  (final, totalTime) <- timeIt $ do
    tSum <- newTVarIO (0 :: Int)
    ch <- newChan
    let spawn i = forkIO $ do
          v <- evaluate (heavyCompute iter i)
          atomically $ modifyTVar' tSum (+ v)
          writeChan ch v
    mapM_ spawn [1 .. n]
    replicateM n (readChan ch)
    final' <- readTVarIO tSum
    final' `deepseq` return final'
  return (final, totalTime)

type Method = (String, Int -> Int -> IO (Int, Double))

availableMethods :: [Method]
availableMethods =
  [ ("Single-threaded (lazy)", \n iter -> runSingleThreadLazy n iter),
    ("Single-threaded (eager)", \n iter -> runSingleThreadEager n iter),
    ("MVar per thread (lazy)", \n iter -> runWithMVarsLazy n iter),
    ("MVar per thread (eager)", \n iter -> runWithMVarsEager n iter),
    ("Chan (lazy)", \n iter -> runWithChanLazy n iter),
    ("Chan (eager)", \n iter -> runWithChanEager n iter),
    ("IORef atomic (lazy)", \n iter -> runWithIORefAtomicLazy n iter),
    ("IORef atomic (eager)", \n iter -> runWithIORefAtomicEager n iter),
    ("STM (TVar) (lazy)", \n iter -> runWithSTMLazy n iter),
    ("STM (TVar) (eager)", \n iter -> runWithSTMEager n iter)
  ]

runAll :: Int -> Int -> IO ()
runAll n iter = do
  putStrLn $ "Benchmark: n = " ++ show n ++ ", iter = " ++ show iter
  let methods = availableMethods
  mapM_ (runAndPrint n iter) methods

runAndPrint :: Int -> Int -> Method -> IO ()
runAndPrint n iter (name, action) = do
  putStrLn $ "\n=== Method: " ++ name ++ " ==="
  (res, totalTime) <- action n iter
  printf "Total sum = %d\n" res
  printf "Total wall time = %.6f s\n" totalTime

main :: IO ()
main = runAll 3000 100000

-- Lazy verssions do not take advantage of multiple threads
-- Benchmark: n = 3000, iter = 100000

-- === Method: Single-threaded (lazy) ===
-- Total sum = 22439212480
-- Total wall time = 30.513029 s

-- === Method: Single-threaded (eager) ===
-- Total sum = 22439212480
-- Total wall time = 29.386858 s

-- === Method: MVar per thread (lazy) ===
-- Total sum = 22439212480
-- Total wall time = 30.853059 s

-- === Method: MVar per thread (eager) ===
-- Total sum = 22439212480
-- Total wall time = 2.473174 s

-- === Method: Chan (lazy) ===
-- Total sum = 22439212480
-- Total wall time = 29.666326 s

-- === Method: Chan (eager) ===
-- Total sum = 22439212480
-- Total wall time = 2.480701 s

-- === Method: IORef atomic (lazy) ===
-- Total sum = 22439212480
-- Total wall time = 29.503403 s

-- === Method: IORef atomic (eager) ===
-- Total sum = 22439212480
-- Total wall time = 2.438593 s

-- === Method: STM (TVar) (lazy) ===
-- Total sum = 22439212480
-- Total wall time = 2.418602 s

-- === Method: STM (TVar) (eager) ===
-- Total sum = 22439212480
-- Total wall time = 2.464690 s