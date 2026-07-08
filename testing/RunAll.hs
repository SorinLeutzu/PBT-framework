module Main where

import Control.Monad (forM, forM_)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Exit (ExitCode(..))
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Process (proc, readCreateProcessWithExitCode)

buildDir :: FilePath
buildDir = "testing/.build"

capture :: String -> [String] -> IO String
capture prog args = do
  (code, out, err) <- readCreateProcessWithExitCode (proc prog args) ""
  let extra = (if null err then "" else "\n[stderr]\n" ++ err)
              ++ (case code of ExitSuccess -> ""; ExitFailure n -> "\n[exit " ++ show n ++ "]\n")
  return (out ++ extra)

runExe :: FilePath -> [String] -> IO String
runExe exe args = do
  abs' <- makeAbsolute exe
  capture abs' args

compileGhc :: [String] -> FilePath -> FilePath -> FilePath -> IO (Either String ())
compileGhc flags outdir exe src = do
  createDirectoryIfMissing True outdir
  (code, _out, err) <-
    readCreateProcessWithExitCode
      (proc "ghc" (flags ++ ["-i.", "-outputdir", outdir, "-o", exe, src])) ""
  return $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left err

testSources :: [(String, FilePath)]
testSources =
  [ ("Generator",      "testing/tests/Generator/GeneratorTests.hs")
  , ("Assertions",     "testing/tests/Assertions/AssertionTests.hs")
  , ("Shrinking",      "testing/tests/Shrinking/ShrinkingTests.hs")
  , ("GrammarFuzzing", "testing/tests/GrammarFuzzing/GrammarFuzzingTests.hs")
  , ("TestTree",       "testing/tests/TestTree/TestTreeTests.hs")
  , ("Grading",        "testing/tests/Grading/GradingTests.hs")
  ]

outputFor :: FilePath -> FilePath
outputFor src = reverse (dropWhile (/= '/') (reverse src)) ++ "output.txt"

runTests :: IO ()
runTests = do
  putStrLn "==== TESTS ===="
  let outdir = buildDir ++ "/tests"
  forM_ testSources $ \(name, src) -> do
    putStrLn ("  compiling test " ++ name)
    let exe = buildDir ++ "/test_" ++ name ++ ".exe"
    result <- compileGhc ["-O1", "-threaded", "-rtsopts"] outdir exe src
    out <- case result of
      Left err -> return ("[compile failed]\n" ++ err)
      Right () -> runExe exe ["+RTS", "-N", "-RTS"]
    writeFile (outputFor src) out
    putStrLn ("  done test " ++ name)

data Bench = Bench
  { bName   :: String
  , bSrc    :: FilePath
  , sInterp :: Int
  , sO0     :: Int
  , sO1     :: Int
  , sO2     :: Int
  }

benches :: [Bench]
benches =
  [ Bench "ParallelSequentialShrinking"
      "testing/benchmarks/ParallelSequentialShrinking/ParallelSequentialShrinking.hs" 50 100 100 100
  , Bench "RandomDataDistributions"
      "testing/benchmarks/RandomDataDistributions/RandomDataDistributions.hs" 25 100 100 100
  , Bench "RandomGeneratorsPerformance"
      "testing/benchmarks/RandomGeneratorsPerformance/RandomGeneratorsPerformance.hs" 100 100 100 100
  , Bench "MemoryUtilization"
      "testing/benchmarks/MemoryUtilization/MemoryUtilization.hs" 25 100 100 100
  , Bench "GrammarFuzzerPerformance"
      "testing/benchmarks/GrammarFuzzerPerformance/GrammarFuzzerPerformance.hs" 100 100 100 100
  ]

runBench :: Bench -> IO ()
runBench b = do
  putStrLn ("  benchmark " ++ bName b)
  compiledOuts <- forM [("O0", "-O0", sO0 b), ("O1", "-O1", sO1 b), ("O2", "-O2", sO2 b)] $
    \(lvl, opt, scale) -> do
      putStrLn ("    compiling " ++ lvl)
      let outdir = buildDir ++ "/" ++ lvl
          exe    = buildDir ++ "/" ++ bName b ++ "_" ++ lvl ++ ".exe"
      result <- compileGhc [opt, "-threaded", "-rtsopts"] outdir exe (bSrc b)
      body <- case result of
        Left err -> return ("[compile failed]\n" ++ err)
        Right () -> runExe exe [show scale, "+RTS", "-N", "-T", "-RTS"]
      return ("==== " ++ lvl ++ " (scale " ++ show scale ++ "%) ====\n" ++ body ++ "\n")
  putStrLn "    interpreted"
  interpBody <- capture "runghc" ["-i.", bSrc b, show (sInterp b)]
  let interpOut = "==== interpreted (scale " ++ show (sInterp b) ++ "%) ====\n" ++ interpBody ++ "\n"
  writeFile (outputFor (bSrc b)) (concat compiledOuts ++ interpOut)
  putStrLn ("  done benchmark " ++ bName b)

runBenches :: IO ()
runBenches = do
  putStrLn "==== BENCHMARKS ===="
  forM_ benches runBench

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  createDirectoryIfMissing True buildDir
  runTests
  runBenches
  putStrLn "All tests and benchmarks have been compiled and run."