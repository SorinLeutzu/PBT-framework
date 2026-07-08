module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (forM)

import Random.Core
import Shrinking.Core (shrink)

report :: String -> [(String, Bool)] -> IO ()
report title checks = do
  putStrLn ("== " ++ title ++ " ==")
  mapM_ (\(name, ok) -> putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)) checks
  let total  = length checks
      passed = length (filter snd checks)
  putStrLn (replicate 50 '-')
  putStrLn ("Passed " ++ show passed ++ " / " ++ show total)

strictlyDecreasing :: Ord a => [a] -> Bool
strictlyDecreasing xs = and (zipWith (>) xs (drop 1 xs))

monotone :: Int -> Bool
monotone x = strictlyDecreasing (map abs (filter (/= 0) (shrink x)))

monotonicityChecks :: [(String, Bool)]
monotonicityChecks =
  [ ("monotone shrink @ " ++ show x, monotone x)
  | x <- [1, 5, 17, 42, 100, 256, 999, 12345, -7, -100] ]

finiteShrink :: Int -> Bool
finiteShrink n = length (shrink n) >= 0

randomInts :: Int -> [Int]
randomInts n = take n (go (PRNG_Xor seedXor))
  where go p = let (v, p') = execRandom p nextInt in v : go p'

terminationChecks :: [(String, Bool)]
terminationChecks =
  [ ("shrink terminates for 1..10000",        all finiteShrink [1 .. 10000])
  , ("shrink terminates for 100 random ints", all finiteShrink (randomInts 100))
  ]

idempotenceChecks :: [(String, Bool)]
idempotenceChecks =
  [ ("minimal Int (0) does not shrink",     null (shrink (0 :: Int)))
  , ("minimal Bool (False) does not shrink", null (shrink False))
  , ("minimal [Int] ([]) does not shrink",   null (shrink ([] :: [Int])))
  , ("minimal Char does not shrink",         null (shrink (minimum printableChars)))
  ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

firstFail :: (a -> Bool) -> [a] -> Maybe a
firstFail _ [] = Nothing
firstFail p (c : cs)
  | not (p c) = Just c
  | otherwise = firstFail p cs

shrinkSeq :: (a -> Bool) -> (a -> [a]) -> a -> a
shrinkSeq p shr x
  | p x       = x
  | otherwise = go x (shr x)
  where
    go lastFail cands = case firstFail p cands of
      Nothing -> lastFail
      Just f  -> go f (shr f)

shrinkDetPar :: NFData a => Int -> (a -> Bool) -> (a -> [a]) -> a -> IO a
shrinkDetPar chunk p shr x
  | p x       = return x
  | otherwise = go x (shr x)
  where
    go lastFail cands = do
      mvars <- forM (chunksOf chunk cands) $ \c -> do
        mv <- newEmptyMVar
        _  <- forkIO (evaluate (force (firstFail p c)) >>= putMVar mv)
        return mv
      res <- readInOrder mvars
      case res of
        Nothing -> return lastFail
        Just f  -> go f (shr f)
    readInOrder [] = return Nothing
    readInOrder (mv : rest) = do
      r <- takeMVar mv
      case r of
        Just f  -> return (Just f)
        Nothing -> readInOrder rest

prop :: Int -> Bool
prop n = n >= 0 && n < 5

startValues :: [Int]
startValues = [1000, 873, 555, 321, 99, 50, 41, 23, 17, 9]

congruenceChecks :: IO [(String, Bool)]
congruenceChecks = forM startValues $ \x -> do
  let seqRes = shrinkSeq prop shrink x
  parRes <- shrinkDetPar 4 prop shrink x
  return ("seq == det-parallel minimal @ " ++ show x, seqRes == parRes)

main :: IO ()
main = do
  congruence <- congruenceChecks
  report "6.1.3 Shrinking"
    (  monotonicityChecks
    ++ terminationChecks
    ++ idempotenceChecks
    ++ congruence
    )