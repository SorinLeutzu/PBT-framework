module TestModules where

import ArbitraryTests
import RandomGenTests
import ShrinkerTests
import TestAssertions

testModules :: IO ()
testModules = do
  putStrLn "Running Random Generation Tests"
  results1 <- mapM runTest allRandomGenTests
  putStrLn ""


  putStrLn "Running Arbitrary Instance Tests"
  results2 <- mapM runTest allArbitraryTests
  putStrLn ""

  putStrLn "Running Shrinker Function Tests"
  results3 <- mapM runTest allShrinkerTests
  putStrLn ""

  let allResults = results1 ++ results2 ++ results3
      total = length allResults
      passed = length (filter id allResults)
      failed = total - passed

  putStrLn "Summary"
  putStrLn $ "Total tests: " ++ show total
  putStrLn $ "Passed: " ++ show passed
  putStrLn $ "Failed: " ++ show failed
  putStrLn ""

  if failed == 0
    then putStrLn "All passed"
    else putStrLn "Some failed"

