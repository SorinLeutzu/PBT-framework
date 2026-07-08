{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Word qualified as W
import Random.Core
import Random.PRNG (nextXor, nextMersenne, pcgAdvance, pcgOutput)

report :: String -> [(String, Bool)] -> IO ()
report title checks = do
  putStrLn ("== " ++ title ++ " ==")
  mapM_ (\(name, ok) -> putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)) checks
  let total  = length checks
      passed = length (filter snd checks)
  putStrLn (replicate 50 '-')
  putStrLn ("Passed " ++ show passed ++ " / " ++ show total)

stepSequence :: (W.Word64 -> W.Word64) -> W.Word64 -> Int -> [W.Word64]
stepSequence step seed n = take n (iterate step seed)

pcgOutputs :: W.Word64 -> Int -> [W.Word64]
pcgOutputs seed n = take n (map pcgOutput (iterate pcgAdvance seed))

genInts :: PRNG -> Int -> Int -> Int -> [Int]
genInts prng lo hi n = take n (go prng)
  where go p = let (v, p') = execRandom p (nextIntRange lo hi) in v : go p'

genDoubles :: PRNG -> Double -> Double -> Int -> [Double]
genDoubles prng lo hi n = take n (go prng)
  where go p = let (v, p') = execRandom p (nextDoubleRange lo hi) in v : go p'

adjacentDiffer :: Eq a => [a] -> Bool
adjacentDiffer xs = and (zipWith (/=) xs (drop 1 xs))

inRange :: Ord a => a -> a -> [a] -> Bool
inRange lo hi = all (\v -> v >= lo && v <= hi)

noShortPeriod :: (W.Word64 -> W.Word64) -> W.Word64 -> Int -> Bool
noShortPeriod step seed n = go (step seed) 1
  where
    go !cur !i
      | i >= n          = True
      | cur == seed     = False
      | otherwise       = go (step cur) (i + 1)

nonDegenSteps, rangeSteps, detSteps, periodSteps :: Int
nonDegenSteps = 1000
rangeSteps    = 500
detSteps      = 500
periodSteps   = 1000000

data Generator = Generator
  { genName :: String
  , values  :: Int -> [W.Word64]
  , altSeed :: Int -> [W.Word64]
  , prng    :: PRNG
  , stepFn  :: W.Word64 -> W.Word64
  , seed    :: W.Word64
  }

generators :: [Generator]
generators =
  [ Generator "XorShift"
      (stepSequence nextXor seedXor)
      (stepSequence nextXor 99999)
      (PRNG_Xor seedXor) nextXor seedXor
  , Generator "Mersenne"
      (stepSequence nextMersenne seedMersenne)
      (stepSequence nextMersenne 12345)
      (PRNG_Mersenne seedMersenne) nextMersenne seedMersenne
  , Generator "PCG"
      (pcgOutputs seedPcg)
      (pcgOutputs 42)
      (PRNG_Pcg seedPcg) pcgAdvance seedPcg
  ]

checksFor :: Generator -> [(String, Bool)]
checksFor g =
  [ (genName g ++ ": non-degeneracy (1000 adjacent values differ)",
       adjacentDiffer (values g nonDegenSteps))
  , (genName g ++ ": determinism (same seed gives identical output)",
       values g detSteps == values g detSteps)
  , (genName g ++ ": different seeds give different output",
       values g detSteps /= altSeed g detSteps)
  , (genName g ++ ": int range [0,100]",
       inRange 0 100 (genInts (prng g) 0 100 rangeSteps))
  , (genName g ++ ": int range [-50,50]",
       inRange (-50) 50 (genInts (prng g) (-50) 50 rangeSteps))
  , (genName g ++ ": double range [-10,10]",
       inRange (-10.0) 10.0 (genDoubles (prng g) (-10.0) 10.0 rangeSteps))
  , (genName g ++ ": double range [5,100]",
       inRange 5.0 100.0 (genDoubles (prng g) 5.0 100.0 rangeSteps))
  , (genName g ++ ": no short period within 10^6 steps",
       noShortPeriod (stepFn g) (seed g) periodSteps)
  ]

main :: IO ()
main = report "6.1.1 Generator" (concatMap checksFor generators)