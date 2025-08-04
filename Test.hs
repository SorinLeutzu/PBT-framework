module Main where

import Assertions

report :: String -> Either String () -> IO ()
report label (Right ()) = putStrLn $ "Non-fatal tests passed: " ++ label
report label (Left err) = putStrLn $ "Non-fatal tests failed" ++ label ++ " : " ++ err

main :: IO ()
main = do
  putStrLn "Running assertions: "
  assertThat (eq (5 :: Int)) (5 :: Int)
  putStrLn "Passed first assertion"

  expectThat (contains "xyz") ("abcxyd") >>= report " contains string: "
  expectThat (contains "xyz") ("bcxyz") >>= report " contains string: "

  let combo :: And (GtMatcher Int) (Not (EqMatcher Int) Int) Int
      combo = gt (8 :: Int) `And` Not (eq (5 :: Int))

  expectThat combo (5 :: Int) >>= report "combo 5" -- this should not work
  expectThat combo (6 :: Int) >>= report "combo 6" -- this should work
  assertThat (gt (10 :: Int)) (50 :: Int)
  putStrLn "Passed second assertion" -- this should not be printed