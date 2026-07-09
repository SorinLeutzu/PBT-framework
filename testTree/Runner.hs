module TestTree.Runner where

import Control.Monad.Writer (runWriter)
import Control.DeepSeq (NFData(..), force)
import Control.Exception (SomeException, handle, evaluate)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Config (ExecutionMode(..), executionMode)
import Execution (assertThenShrinkRaw, parallelMap)
import Matchers.Core
import Random.Core qualified as Rand
import PrettyPrinting.Renders
import TestTree.Types

{-# NOINLINE runTestCase #-}
runTestCase :: TestCase -> TestOutcome
runTestCase tc = unsafePerformIO $ handle exceptionHandler $ do
  result <- timeout 2000000 (evaluate (force (runTestCasePure tc)))
  return $ case result of
    Nothing  -> TestFail "Timeout: test exceeded 2 seconds"
    Just out -> out
  where
    exceptionHandler :: SomeException -> IO TestOutcome
    exceptionHandler err = return $ TestFail ("Error: " ++ show err)

runTestCasePure :: TestCase -> TestOutcome

runTestCasePure (UnitCase _ m x) =
  let ok = matches m x
      expl = explainMatch m x
  in if ok then TestPass expl else TestFail expl

runTestCasePure (FuzzCase name matcher shrinker iters) =
  let loop i prng
        | i == iters = TestPass ("All " ++ show iters ++ " cases passed")
        | otherwise =
            let (x, prng') = Rand.runRandom (Rand.arbitrary ()) prng
            in if matches matcher x
                 then loop (i + 1) prng'
                 else
                   let (maybeMin, logs) = runWriter (assertThenShrinkRaw x matcher shrinker)
                       rendered = renderLogs logs
                       header = case maybeMin of
                         Nothing -> "Initial failure reported but matched after shrinking (unexpected)"
                         Just minimal -> "Found counterexample; minimal = " ++ show minimal
                       info = "Fuzz failure at iteration " ++ show i ++ " of " ++ show iters ++ " with input " ++ show x
                       body = unlines (map ("  " ++) rendered)
                   in TestFail (header ++ "\n" ++ info ++ "\n" ++ body)
  in loop 0 (Rand.PRNG_Xor Rand.seedXor)

runTestCasePure (FuzzGenCase name generator prop iters) =
  let pm = PredMatcher {predFn = prop, predName = "property"}
      loop i prng
        | i == iters = TestPass ("All " ++ show iters ++ " cases passed")
        | otherwise =
            let (x, prng') = Rand.runRandom (genA generator) prng
            in if matches pm x
                 then loop (i + 1) prng'
                 else
                   let (maybeMin, logs) = runWriter (assertThenShrinkRaw x pm (shrinkA generator))
                       rendered = renderLogs logs
                       header = case maybeMin of
                         Nothing -> "Initial failure reported but matched after shrinking (unexpected)"
                         Just minimal -> "Found counterexample; minimal = " ++ show minimal
                       info = "Fuzz failure at iteration " ++ show i ++ " of " ++ show iters ++ " with input " ++ show x
                       body = unlines (map ("  " ++) rendered)
                   in TestFail (header ++ "\n" ++ info ++ "\n" ++ body)
  in loop 0 (Rand.PRNG_Xor Rand.seedXor)


data TestLeaf = TestLeaf Double TestCase

instance NFData TestLeaf where
  rnf (TestLeaf w tc) = w `seq` rnf (runTestCase tc) `seq` ()

instance NFData TestOutcome where
  rnf (TestPass s) = rnf s
  rnf (TestFail s) = rnf s

collectLeaves :: TestTree -> [TestLeaf]
collectLeaves (Test weight tc) = [TestLeaf weight tc]
collectLeaves (Describe _ _ children) = concatMap collectLeaves children

executeTestTree :: TestTree -> TestResultTree
executeTestTree tree =
  let leaves = collectLeaves tree
      runLeaf (TestLeaf _ tc) = runTestCase tc
      outcomes = case executionMode of
        ParallelExec   -> parallelMap runLeaf leaves
        SequentialExec -> map runLeaf leaves
  in rebuildTree tree (zip leaves outcomes)

rebuildTree :: TestTree -> [(TestLeaf, TestOutcome)] -> TestResultTree
rebuildTree tree pairs = fst (go tree pairs)
  where
    go (Test _ tc) ((_, outcome):rest) = (ResultLeaf (caseName tc) outcome, rest)
    go (Test _ tc) [] = (ResultLeaf (caseName tc) (TestFail "internal error: no outcome"), [])
    go (Describe _ name children) ps =
      let (childResults, remaining) = foldl step ([], ps) children
          step (acc, ps') child =
            let (r, ps'') = go child ps'
            in (acc ++ [r], ps'')
      in (ResultNode name childResults, remaining)


countResults :: TestResultTree -> (Int, Int)
countResults (ResultLeaf _ (TestPass _)) = (1, 0)
countResults (ResultLeaf _ (TestFail _)) = (0, 1)
countResults (ResultNode _ children) =
  foldl (\(p1, f1) c -> let (p2, f2) = countResults c in (p1 + p2, f1 + f2)) (0, 0) children


renderResults :: TestResultTree -> IO (Int, Int)
renderResults = go 0
  where
    go indent (ResultNode name children) = do
      putStrLn (renderSuiteHeader indent name)
      pairs <- mapM (go (indent + 2)) children
      let (p, f) = foldl (\(p1, f1) (p2, f2) -> (p1 + p2, f1 + f2)) (0, 0) pairs
      pure (p, f)
    go indent (ResultLeaf name outcome) = do
      case outcome of
        TestPass msg -> do
          putStrLn (renderPassLine indent name msg)
          pure (1, 0)
        TestFail msg -> do
          putStrLn (renderFailLine indent name msg)
          pure (0, 1)

runTestTree :: TestTree -> IO ()
runTestTree tt = do
  let results = executeTestTree tt
  putStrLn ""
  _ <- renderResults results
  putStrLn ""
  let suiteStats = case results of
        ResultNode _ children ->
          map (\c -> let (p, f) = countResults c in (resultName c, p, f)) children
        leaf -> let (p, f) = countResults leaf in [(resultName leaf, p, f)]
      rows = map (\(name, p, f) -> [name, show p, show f, show (p + f)]) suiteStats
      (tp, tf) = foldl (\(ap, af) (_, p, f) -> (ap + p, af + f)) (0, 0) suiteStats
      totalRow = [condBold "Total", condBold (show tp), condBold (show tf), condBold (show (tp + tf))]
  putStr $ renderTable Table
    { tHeaders = ["Suite", "Passed", "Failed", "Total"]
    , tRows    = rows ++ [totalRow]
    }