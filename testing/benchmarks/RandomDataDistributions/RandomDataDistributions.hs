module Main where

import Data.List (sort, group)
import System.Environment (getArgs)
import Text.Printf (printf)

import Random.Core
import PrettyPrinting.Renders (Table(..), renderTable)

readScale :: IO Int
readScale = do
  args <- getArgs
  return $ case args of
    (s : _) -> max 1 (read s)
    _       -> 100

drawInts :: PRNG -> Int -> Int -> Int -> [Int]
drawInts prng lo hi n = take n (go prng)
  where go p = let (v, p') = execRandom p (nextIntRange lo hi) in v : go p'

drawBools :: PRNG -> Int -> [Bool]
drawBools prng n = take n (go prng)
  where go p = let (v, p') = execRandom p nextBool in v : go p'

histogram :: Int -> Int -> [Int] -> [(Int, Int)]
histogram lo hi xs =
  let counted = map (\g -> (head g, length g)) (group (sort xs))
  in [ (b, maybe 0 id (lookup b counted)) | b <- [lo .. hi] ]

printIntHistogram :: String -> Int -> Int -> [Int] -> IO ()
printIntHistogram title lo hi xs = do
  putStrLn title
  let total = length xs
      rows  = [ [ show b, show c, printf "%.2f%%" (100 * fromIntegral c / fromIntegral total :: Double) ]
              | (b, c) <- histogram lo hi xs ]
  putStr (renderTable Table { tHeaders = ["value", "count", "share"], tRows = rows })
  putStrLn ""

main :: IO ()
main = do
  scale <- readScale
  let sampleCount = max 1 (100000 * scale `div` 100)
  putStrLn ("Scale " ++ show scale ++ "%: approximating distributions with "
            ++ show sampleCount ++ " samples each")
  putStrLn ""

  printIntHistogram "Int in [0,9] (XorShift)" 0 9
    (drawInts (PRNG_Xor seedXor) 0 9 sampleCount)
  printIntHistogram "Int in [0,9] (Mersenne)" 0 9
    (drawInts (PRNG_Mersenne seedMersenne) 0 9 sampleCount)
  printIntHistogram "Int in [0,9] (PCG)" 0 9
    (drawInts (PRNG_Pcg seedPcg) 0 9 sampleCount)

  printIntHistogram "Die roll in [1,6] (XorShift)" 1 6
    (drawInts (PRNG_Xor seedXor) 1 6 sampleCount)

  printIntHistogram "Bool as 0/1 (XorShift)" 0 1
    (map (\b -> if b then 1 else 0) (drawBools (PRNG_Xor seedXor) sampleCount))