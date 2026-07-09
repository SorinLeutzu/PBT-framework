module Grading where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Config (withEmojis)
import PrettyPrinting.Emojis (EmojiName (..), emoji)
import PrettyPrinting.Renders
import TestTree
import Text.Printf (printf)

newtype Grade = Grade Double
  deriving (Eq, Show)

data GradedResult
  = GradedNode String Grade Bool [GradedResult]
  | GradedLeaf String Double TestOutcome Grade


gradeTreeFrom :: TestTree -> [TestOutcome] -> (GradedResult, [TestOutcome])
gradeTreeFrom (Describe policy name children) outs =
  (GradedNode name totalGrade allPass gradedChildren, rest)
  where
    (gradedChildren, rest) = foldl step ([], outs) children
    step (acc, os) child =
      let (r, os') = gradeTreeFrom child os
      in (acc ++ [r], os')
    passed = map resultPassed gradedChildren
    allPass = case policy of
      AllPass      -> and passed
      MajorityPass -> 2 * length (filter id passed) >= length passed
      AnyPass      -> or passed
    grades = [g | Grade g <- map resultGrade gradedChildren]
    totalGrade
      | null grades                     = Grade 0.0
      | all isGradedLeaf gradedChildren = Grade (sum grades)
      | otherwise                       = Grade (sum grades / fromIntegral (length grades))
gradeTreeFrom (Test weight tc) (o : rest) =
  (GradedLeaf (caseName tc) weight o grade, rest)
  where
    grade = case o of
      TestPass _ -> Grade weight
      TestFail _ -> Grade 0.0
gradeTreeFrom (Test weight tc) [] =
  (GradedLeaf (caseName tc) weight (TestFail "internal error: no outcome") (Grade 0.0), [])

gradeTree :: TestTree -> GradedResult
gradeTree tree = fst (gradeTreeFrom tree outcomes)
  where
    outcomes = map (\(TestLeaf _ tc) -> runTestCase tc) (collectLeaves tree)

resultPassed :: GradedResult -> Bool
resultPassed (GradedNode _ _ p _) = p
resultPassed (GradedLeaf _ _ (TestPass _) _) = True
resultPassed _ = False

resultGrade :: GradedResult -> Grade
resultGrade (GradedNode _ g _ _) = g
resultGrade (GradedLeaf _ _ _ g) = g

gradedName :: GradedResult -> String
gradedName (GradedNode name _ _ _) = name
gradedName (GradedLeaf name _ _ _) = name

isGradedLeaf :: GradedResult -> Bool
isGradedLeaf GradedLeaf {} = True
isGradedLeaf _ = False

passLabel :: Bool -> String
passLabel True  = "PASS"
passLabel False = "FAIL"

runLeavesWithProgress :: [TestLeaf] -> IO [TestOutcome]
runLeavesWithProgress leaves = go 0 leaves
  where
    total = length leaves
    barWidth = 30
    labelWidth = 42

    bar current label = ProgressBar total current barWidth (progressLabel label)

    go done [] = do
      renderProgressBar (bar done "all tests executed")
      putStrLn ""
      pure []
    go done (TestLeaf _ tc : rest) = do
      renderProgressBar (bar done (caseName tc))
      out <- evaluate (force (runTestCase tc))
      (out :) <$> go (done + 1) rest

    progressLabel name =
      let t = if length name > labelWidth
              then take (labelWidth - 3) name ++ "..."
              else name
      in t ++ replicate (labelWidth - length t) ' ' ++ " "


fmt2 :: Double -> String
fmt2 = printf "%.2f"

statusText :: Bool -> String
statusText True  = condGreen "PASS"
statusText False = condRed "FAIL"

showGradedResult :: GradedResult -> String
showGradedResult = renderAt 0
  where
    renderAt indent (GradedNode name (Grade g) allPass children) =
      renderSuiteHeader indent name
        ++ " " ++ statusText allPass
        ++ " [" ++ condBold (fmt2 g ++ " pts") ++ "]\n"
        ++ concatMap (renderAt (indent + 2)) children

    renderAt indent (GradedLeaf name weight outcome (Grade g)) =
      let title = name ++ " (weight " ++ fmt2 weight ++ "%) [" ++ fmt2 g ++ " pts]"
      in case outcome of
           TestPass msg -> renderPassLine indent title (firstLine msg) ++ "\n"
           TestFail msg -> renderFailLine indent title (firstLine msg) ++ "\n"

    firstLine = takeWhile (/= '\n')

printFinalGrade :: Grade -> GradedResult -> IO ()
printFinalGrade (Grade grade) result = do
  let passed = resultPassed result
      statusEmoji
        | not withEmojis        = ""
        | not passed            = " " ++ emoji Alarm
        | grade >= 100.0 - 1e-9 = " " ++ emoji AllPoints
        | otherwise             = " " ++ emoji Happy
  putStrLn ""
  putStrLn (renderSuiteHeader 0 "Final Grade")
  putStrLn ("  Grade:  " ++ condBold (fmt2 grade ++ " / 100.00"))
  putStrLn ("  Status: " ++ statusText passed ++ statusEmoji)
  putStrLn ""

gradeSummaryTable :: GradedResult -> String
gradeSummaryTable result =
  renderTable Table
    { tHeaders = ["Suite", "Grade", "Status"]
    , tRows    = map row children ++ [totalRow]
    }
  where
    children = case result of
      GradedNode _ _ _ cs -> cs
      leaf                -> [leaf]
    row r =
      let Grade g = resultGrade r
      in [gradedName r, fmt2 g, statusText (resultPassed r)]
    Grade total = resultGrade result
    totalRow = [condBold "Total", condBold (fmt2 total), statusText (resultPassed result)]

gradeAndPrint :: TestTree -> IO ()
gradeAndPrint tree = do
  outcomes <- runLeavesWithProgress (collectLeaves tree)
  let (result, _) = gradeTreeFrom tree outcomes
  printFinalGrade (resultGrade result) result
  putStrLn (renderSuiteHeader 0 "Detailed Results")
  putStrLn ""
  putStrLn (showGradedResult result)
  putStr (gradeSummaryTable result)

testGrading :: IO ()
testGrading = do
  putStrLn "Testing grading functionality"
  putStrLn ""
  putStrLn "Test 1: Example Tree"
  gradeAndPrint exampleTree
  putStrLn ""
  putStrLn "Test 2: Failing Tree"
  gradeAndPrint exampleTreeFailing
  putStrLn ""
  putStrLn "Test 3: Nested Tree"
  gradeAndPrint exampleTreeNested
  putStrLn ""
  putStrLn "Grading tests complete"

main :: IO ()
main = testGrading