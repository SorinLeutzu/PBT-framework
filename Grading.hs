module Grading where

import TestTree
import Text.Printf (printf)

newtype Grade = Grade Double
  deriving (Eq, Show)

data GradedResult
  = GradedNode String Grade Bool [GradedResult]
  | GradedLeaf String Double TestOutcome Grade

gradeTree :: TestTree -> GradedResult
gradeTree (Describe policy name children) =
  GradedNode name totalGrade allPass gradedChildren
  where
    gradedChildren = map gradeTree children
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
gradeTree (Test weight tc) =
  GradedLeaf (caseName tc) weight outcome grade
  where
    outcome = runTestCase tc
    grade = case outcome of
      TestPass _ -> Grade weight
      TestFail _ -> Grade 0.0

resultPassed :: GradedResult -> Bool
resultPassed (GradedNode _ _ p _) = p
resultPassed (GradedLeaf _ _ (TestPass _) _) = True
resultPassed _ = False

resultGrade :: GradedResult -> Grade
resultGrade (GradedNode _ g _ _) = g
resultGrade (GradedLeaf _ _ _ g) = g

isGradedLeaf :: GradedResult -> Bool
isGradedLeaf GradedLeaf {} = True
isGradedLeaf _ = False

passLabel :: Bool -> String
passLabel True  = "PASS"
passLabel False = "FAIL"

showGradedResult :: GradedResult -> String
showGradedResult = renderAt 0
  where
    renderAt indent (GradedNode name (Grade g) allPass children) = pad indent ++ "Suite: " ++ name ++ " [Grade: " ++ printf "%.2f" g
        ++ ", " ++ passLabel allPass ++ "]\n" ++ concatMap (renderAt (indent + 2)) children

    renderAt indent (GradedLeaf name weight outcome (Grade g)) =  pad indent ++ "Test: " ++ name ++ " (weight " ++ printf "%.2f" weight ++ "%)"
        ++ " -> " ++ showOutcome outcome ++ " [" ++ printf "%.2f" g ++ " pts]\n"

    showOutcome (TestPass msg) = "PASS: " ++ firstLine msg
    showOutcome (TestFail msg) = "FAIL: " ++ firstLine msg
    firstLine = takeWhile (/= '\n')
    pad n = replicate n ' '

printFinalGrade :: Grade -> GradedResult -> IO ()
printFinalGrade (Grade grade) result = do
  putStrLn ""
  putStrLn "Final Grade"
  printf "Grade: %.2f / 100.00\n" grade
  putStrLn ("Status: " ++ passLabel (resultPassed result))
  putStrLn ""

gradeAndPrint :: TestTree -> IO ()
gradeAndPrint tree = do
  let result = gradeTree tree
  printFinalGrade (resultGrade result) result
  putStrLn "Detailed Results:"
  putStrLn (showGradedResult result)

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