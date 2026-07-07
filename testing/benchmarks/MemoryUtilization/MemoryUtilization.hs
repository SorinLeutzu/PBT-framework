module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (forM)
import GHC.Stats
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

import Shrinking.Core (shrink)
import GrammarFuzzer.GrammarDefinitions (Grammar'(..))
import GrammarFuzzer.Construction (constructParseTree)
import GrammarFuzzer.Display (evaluateParseTree)
import GrammarFuzzer.Examples (grammarArithmetic')
import PrettyPrinting.Renders (Table(..), renderTable)

measure :: NFData a => a -> IO (Double, Double)
measure x = do
  performGC
  s0 <- getRTSStats
  _  <- evaluate (force x)
  performGC
  s1 <- getRTSStats
  let mb b = fromIntegral b / (1024 * 1024) :: Double
      alloc = mb (allocated_bytes s1 - allocated_bytes s0)
      live  = mb (max_live_bytes s1)
  return (alloc, live)

shrinkWork :: Int -> Int
shrinkWork n = sum (map length (shrink [1 .. n]))

grammarWork :: Int -> Int
grammarWork k = length (evaluateParseTree (constructParseTree (Grammar' grammarArithmetic' k k)))

runTable :: String -> (Int -> Int) -> [Int] -> IO ()
runTable title work sizes = do
  putStrLn title
  rows <- forM sizes $ \sz -> do
    (alloc, live) <- measure (work sz)
    return [ show sz, printf "%.2f" alloc, printf "%.2f" live ]
  putStr (renderTable Table
    { tHeaders = ["size", "allocated MB", "live MB"], tRows = rows })
  putStrLn ""

readScale :: IO Int
readScale = do
  args <- getArgs
  return $ case args of
    (s : _) -> max 1 (read s)
    _       -> 100

main :: IO ()
main = do
  scale <- readScale
  let unitShrink = max 1 (1000 * scale `div` 100)
      shrinkSizes = [ unitShrink * k | k <- [1 .. 10] ]
      unitGram = max 1 (2 * scale `div` 100)
      grammarSizes = [ unitGram * k | k <- [1 .. 10] ]
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "RTS stats disabled. Re-run with: +RTS -T -RTS"
    else do
      putStrLn ("Scale " ++ show scale ++ "%: memory utilization across 10 increasing input sizes")
      putStrLn ""
      runTable "Shrinking (list length)" shrinkWork shrinkSizes
      runTable "Grammar string generation (minNonTerminals = randomExpansions)"
        grammarWork grammarSizes