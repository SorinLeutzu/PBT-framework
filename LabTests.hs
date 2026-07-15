module LabTests where

import Lab
import TestTree
import Grading
import Matchers.Core
import Data.List (intercalate, isSubsequenceOf)
import Random.Core qualified as Rand
import Shrinking.Core qualified as Shrink
import GrammarFuzzer.GrammarDefinitions
  ( Grammar (..), Grammar' (..), Symbol (..), GrammarFuzzedString (..), s )
import GrammarFuzzer.Construction (nextGrammarFuzzedString)
import PrettyPrinting.TerminalSetup (setupTerminalWithChcp)




data MeanEvenMatcher = MeanEvenMatcher

refMeanEven :: [Int] -> Double
refMeanEven xs = case filter even xs of
  []    -> 0.0
  evens -> fromIntegral (sum evens) / fromIntegral (length evens)


approxEq :: Double -> Double -> Bool
approxEq x y = abs (x - y) <= 1e-9 * (1 `max` abs x `max` abs y)

instance Matcher MeanEvenMatcher where
  describe _ ok
    | ok = "matches the reference implementation of meanEven"
    | otherwise = "does not match the reference implementation of meanEven"

instance Matchable MeanEvenMatcher [Int] where
  matches _ xs = approxEq (meanEven xs) (refMeanEven xs)
  explainMatch _ xs
    | approxEq actual expected = "which correctly produces " ++ show expected
    | isNaN actual =
        "the result is NaN: the input has " ++ show (length evens)
          ++ " even elements, so the division is 0/0"
          ++ " - return 0.0 when there are no even elements"
    | null evens =
        "the input has no even elements, so the result must be 0.0, but got "
          ++ show actual
    | approxEq actual (meanOf xs) =
        "got " ++ show actual ++ ", which is the mean of ALL " ++ show (length xs)
          ++ " elements - only the " ++ show (length evens) ++ " even elements "
          ++ show evens ++ " should be averaged, giving " ++ show expected
    | approxEq actual (meanOf (filter odd xs)) =
        "got " ++ show actual ++ ", which is the mean of the ODD elements "
          ++ show (filter odd xs) ++ " - the even elements " ++ show evens
          ++ " should be averaged instead, giving " ++ show expected
    | approxEq actual (fromIntegral (sum evens)) =
        "got " ++ show actual ++ ", which is the SUM of the even elements "
          ++ show evens ++ " - divide it by their count " ++ show (length evens)
          ++ " to get the mean " ++ show expected
    | otherwise =
        "the even elements are " ++ show evens ++ " with sum " ++ show (sum evens)
          ++ " and count " ++ show (length evens) ++ ", so the mean is "
          ++ show expected ++ ", but got " ++ show actual
    where
      expected = refMeanEven xs
      actual   = meanEven xs
      evens    = filter even xs
      meanOf ys = fromIntegral (sum ys) / fromIntegral (length ys) :: Double

-- ex 2

data UniqueMatcher = UniqueMatcher

refUnique :: Eq a => [a] -> [a]
refUnique [] = []
refUnique (x:xs) = x : refUnique (filter (/= x) xs)

instance Matcher UniqueMatcher where
  describe _ ok
    | ok = "matches the reference implementation of unique"
    | otherwise = "does not match the reference implementation of unique"

instance (Eq a, Show a) => Matchable UniqueMatcher [a] where
  matches _ xs = unique xs == refUnique xs
  explainMatch _ xs
    | actual == expected = "which correctly produces " ++ show expected
    | not (null duplicated) =
        "the output " ++ show actual ++ " still contains duplicates: "
          ++ intercalate ", "
               [ show e ++ " appears " ++ show (occurrences e actual) ++ " times"
               | e <- duplicated ]
          ++ " - every element must appear exactly once"
    | not (null missing) =
        "the output " ++ show actual ++ " is missing " ++ show missing
          ++ " even though the input " ++ show xs ++ " contains them"
    | not (null invented) =
        "the output " ++ show actual ++ " contains " ++ show invented
          ++ " which do not appear in the input " ++ show xs ++ " at all"
    | otherwise =
        "the output " ++ show actual ++ " has the right elements, each exactly once,"
          ++ " but in the wrong order:  elements must keep the order of their first"
          ++ " occurrence in the input, so expected " ++ show expected
    where
      expected = refUnique xs
      actual   = unique xs
      occurrences e = length . filter (== e)
      duplicated = [ e | e <- refUnique actual, occurrences e actual > 1 ]
      missing    = [ e | e <- expected, e `notElem` actual ]
      invented   = [ e | e <- refUnique actual, e `notElem` xs ]


-- ex 3

data Interleave3Matcher = Interleave3Matcher

refInterleave3 :: [a] -> [a] -> [a] -> [a]
refInterleave3 [] [] [] = []
refInterleave3 xs ys zs =
  take 1 xs ++ take 1 ys ++ take 1 zs
    ++ refInterleave3 (drop 1 xs) (drop 1 ys) (drop 1 zs)

instance Matcher Interleave3Matcher where
  describe _ ok
    | ok = "matches the reference implementation of interleave3"
    | otherwise = "does not match the reference implementation of interleave3"

instance (Eq a, Show a) => Matchable Interleave3Matcher ([a], [a], [a]) where
  matches _ (xs, ys, zs) = interleave3 xs ys zs == refInterleave3 xs ys zs
  explainMatch _ (xs, ys, zs)
    | actual == expected = "which correctly produces " ++ show expected
    | length actual /= total =
        "the interleaved list contains " ++ show (length actual)
          ++ " element(s), but the 3 input lists contain a total of " ++ show total
          ++ " element(s) - when a list runs out, the remaining lists must keep"
          ++ " being interleaved"
    | not (null wrongCount) =
        let e = head wrongCount
        in "the element " ++ show e ++ " appears "
             ++ show (occurrences e pooled) ++ " times across the input lists but "
             ++ show (occurrences e actual) ++ " times in the output " ++ show actual
    | otherwise =
        "all input elements are present, but the order is wrong: at position "
          ++ show firstDiff ++ " expected " ++ show (expected !! firstDiff)
          ++ " but got " ++ show (actual !! firstDiff)
          ++ " (elements must be taken one at a time from each list in turn,"
          ++ " giving " ++ show expected ++ ")"
    where
      expected = refInterleave3 xs ys zs
      actual   = interleave3 xs ys zs
      total    = length xs + length ys + length zs
      pooled   = xs ++ ys ++ zs
      occurrences e = length . filter (== e)
      wrongCount = [ e | e <- pooled, occurrences e actual /= occurrences e pooled ]
      firstDiff = length (takeWhile id (zipWith (==) actual expected))


-- ex 4

data BalancedMatcher = BalancedMatcher

refBalanced :: String -> Bool
refBalanced = go []
  where
    go []    []     = True
    go _     []     = False
    go stack (c:cs)
      | c == '(' || c == '[' || c == '{' = go (c : stack) cs
      | c == ')' = case stack of { '(' : rest -> go rest cs; _ -> False }
      | c == ']' = case stack of { '[' : rest -> go rest cs; _ -> False }
      | c == '}' = case stack of { '{' : rest -> go rest cs; _ -> False }
      | otherwise = go stack cs

instance Matcher BalancedMatcher where
  describe _ ok
    | ok = "matches the reference implementation of balanced"
    | otherwise = "does not match the reference implementation of balanced"


bracketDiagnosis :: String -> String
bracketDiagnosis str = go [] (zip [0 :: Int ..] str)
  where
    closerOf c = lookup c [(')', '('), (']', '['), ('}', '{')]
    go stack [] = case stack of
      [] -> "every bracket is opened and closed correctly"
      ((o, j) : _) ->
        "the " ++ show o ++ " opened at position " ++ show j ++ " is never closed"
    go stack ((i, c) : rest)
      | c `elem` "([{" = go ((c, i) : stack) rest
      | Just opener <- closerOf c = case stack of
          [] ->
            "the " ++ show c ++ " at position " ++ show i
              ++ " closes nothing - no bracket is open at that point"
          ((o, j) : stack')
            | o == opener -> go stack' rest
            | otherwise ->
                "the " ++ show c ++ " at position " ++ show i
                  ++ " does not match the " ++ show o ++ " opened at position "
                  ++ show j ++ " - bracket types must match, not just counts"
      | otherwise = go stack rest

instance Matchable BalancedMatcher String where
  matches _ str = balanced str == refBalanced str
  explainMatch _ str
    | actual == expected =
        "which correctly returns " ++ show expected ++ " for " ++ show str
    | expected =
        "got False, but " ++ show str ++ " IS balanced: " ++ bracketDiagnosis str
    | otherwise =
        "got True, but " ++ show str ++ " is NOT balanced: " ++ bracketDiagnosis str
    where
      expected = refBalanced str
      actual   = balanced str

--  ex 5

data SplitOnMatcher = SplitOnMatcher

refSplitOn :: Eq a => a -> [a] -> [[a]]
refSplitOn _ [] = [[]]
refSplitOn d xs = case break (== d) xs of
  (chunk, [])     -> [chunk]
  (chunk, _:rest) -> chunk : refSplitOn d rest

instance Matcher SplitOnMatcher where
  describe _ ok
    | ok = "matches the reference implementation of splitOn"
    | otherwise = "does not match the reference implementation of splitOn"

instance (Eq a, Show a) => Matchable SplitOnMatcher (a, [a]) where
  matches _ (d, xs) = splitOn d xs == refSplitOn d xs
  explainMatch _ (d, xs)
    | actual == expected = "which correctly produces " ++ show expected
    | length actual /= nDelims + 1 =
        "the input " ++ show xs ++ " contains " ++ show nDelims
          ++ " delimiter(s), so the result must have " ++ show (nDelims + 1)
          ++ " chunk(s), but the output " ++ show actual ++ " has "
          ++ show (length actual)
          ++ " - remember that leading, trailing and adjacent delimiters"
          ++ " produce empty chunks"
    | not (null dirtyChunks) =
        let i = head dirtyChunks
        in "chunk " ++ show i ++ " of the output, " ++ show (actual !! i)
             ++ ", still contains the delimiter " ++ show d
             ++ " - the delimiter must not appear in the result"
    | otherwise =
        let i = length (takeWhile id (zipWith (==) actual expected))
        in "the output has the right number of chunks, but chunk " ++ show i
             ++ " should be " ++ show (expected !! i) ++ " and is "
             ++ show (actual !! i) ++ " (expected " ++ show expected
             ++ ", got " ++ show actual ++ ")"
    where
      expected = refSplitOn d xs
      actual   = splitOn d xs
      nDelims  = length (filter (== d) xs)
      dirtyChunks = [ i | (i, chunk) <- zip [0 :: Int ..] actual, d `elem` chunk ]


-- generators

smallIntListGen :: Generator [Int]
smallIntListGen = Generator
  { genA = do
      n <- Rand.nextIntRange 0 6
      sequenceA (replicate n (Rand.nextIntRange 0 9))
  , shrinkA = Shrink.shrink
  }


grammarStringGen :: Grammar -> Int -> Int -> Generator String
grammarStringGen g minNT rndExp = Generator
  { genA = getGrammarFuzzedString <$> nextGrammarFuzzedString (Grammar' g minNT rndExp)
  , shrinkA = Shrink.shrink
  }



grammarBrackets :: Grammar
grammarBrackets = Grammar
  [ (N "S", [ [s (T "("), s (N "S")], [s (T ")"), s (N "S")]
            , [s (T "["), s (N "S")], [s (T "]"), s (N "S")]
            , [s (T "{"), s (N "S")], [s (T "}"), s (N "S")]
            , [s (T "(")], [s (T ")")]
            , [s (T "[")], [s (T "]")]
            , [s (T "{")], [s (T "}")]
            ])
  ] (N "S")



grammarBalanced :: Grammar
grammarBalanced = Grammar
  [ (N "B", [ [s (T "("), s (N "B"), s (T ")"), s (N "B")]
            , [s (T "["), s (N "B"), s (T "]"), s (N "B")]
            , [s (T "{"), s (N "B"), s (T "}"), s (N "B")]
            , [s (T "()")], [s (T "[]")], [s (T "{}")]
            ])
  ] (N "B")


grammarCsv :: Grammar
grammarCsv = Grammar
  [ (N "S", [ [s (T "a"), s (N "S")]
            , [s (T "b"), s (N "S")]
            , [s (T ","), s (N "S")]
            , [s (T "a")], [s (T "b")], [s (T ",")]
            ])
  ] (N "S")


csvInputGen :: Generator (Char, String)
csvInputGen = Generator
  { genA = (\str -> (',', str)) <$> genA base
  , shrinkA = \(d, str) -> [ (d, str') | str' <- shrinkA base str ]
  }
  where base = grammarStringGen grammarCsv 8 10


-- graded tree option 1

labTree :: TestTree
labTree =
  group AnyPass "Haskell Lab Exam"
    [ group AllPass "Exercise 1: meanEven"
        [ testCase 25.0
            "meanEven [2,4,6]"
            MeanEvenMatcher
            ([2, 4, 6] :: [Int])
        , testCase 25.0
            "meanEven [1,3,5] (no even elements)"
            MeanEvenMatcher
            ([1, 3, 5] :: [Int])
        , fuzz 50.0
            "meanEven matches reference on random inputs"
            MeanEvenMatcher
            (Shrink.shrink :: [Int] -> [[Int]])
            200
        ]

    , group AllPass "Exercise 2: unique"
        [ testCase 25.0
            "unique [1,2,1,3,2]"
            UniqueMatcher
            ([1, 2, 1, 3, 2] :: [Int])
        , testCase 25.0
            "unique \"abcabc\""
            UniqueMatcher
            "abcabc"
        , fuzzGenWith 50.0
            "unique matches reference on random small-int lists"
            smallIntListGen
            UniqueMatcher
            200
        ]

    , group AllPass "Exercise 3: interleave3"
        [ testCase 25.0
            "interleave3 [1,2,3] [4,5,6] [7,8,9]"
            Interleave3Matcher
            (([1, 2, 3], [4, 5, 6], [7, 8, 9]) :: ([Int], [Int], [Int]))
        , testCase 25.0
            "interleave3 [1,2] [3] [4,5,6] (unequal lengths)"
            Interleave3Matcher
            (([1, 2], [3], [4, 5, 6]) :: ([Int], [Int], [Int]))
        , fuzz 50.0
            "interleave3 matches reference on random inputs"
            Interleave3Matcher
            (Shrink.shrink :: ([Int], [Int], [Int]) -> [([Int], [Int], [Int])])
            200
        ]

    , group AllPass "Exercise 4: balanced"
        [ testCase 25.0
            "balanced \"([]){}\""
            BalancedMatcher
            "([]){}"
        , testCase 25.0
            "balanced \"([)]\""
            BalancedMatcher
            "([)]"
        , fuzzGenWith 25.0
            "balanced matches reference on grammar-generated balanced strings"
            (grammarStringGen grammarBalanced 6 8)
            BalancedMatcher
            100
        , fuzzGenWith 25.0
            "balanced matches reference on grammar-generated bracket strings"
            (grammarStringGen grammarBrackets 8 10)
            BalancedMatcher
            100
        ]

    , group AllPass "Exercise 5: splitOn"
        [ testCase 25.0
            "splitOn ',' \"a,b,c\""
            SplitOnMatcher
            (',', "a,b,c")
        , testCase 25.0
            "splitOn ',' \",,\""
            SplitOnMatcher
            (',', ",,")
        , fuzzGenWith 50.0
            "splitOn matches reference on grammar-generated strings"
            csvInputGen
            SplitOnMatcher
            100
        ]
    ]

-- option 2

labTree2 :: TestTree
labTree2 =
  group AnyPass "Haskell Lab Exam (mixed grading policies)"
    [ group AnyPass "Exercise 1: meanEven [AnyPass]"
        [ testCase 25.0
            "meanEven [2,4,6]"
            MeanEvenMatcher
            ([2, 4, 6] :: [Int])
        , testCase 25.0
            "meanEven [1,3,5] (no even elements)"
            MeanEvenMatcher
            ([1, 3, 5] :: [Int])
        , fuzz 50.0
            "meanEven matches reference on random inputs"
            MeanEvenMatcher
            (Shrink.shrink :: [Int] -> [[Int]])
            200
        ]

    , group MajorityPass "Exercise 2: unique [MajorityPass]"
        [ testCase 25.0
            "unique [1,2,1,3,2]"
            UniqueMatcher
            ([1, 2, 1, 3, 2] :: [Int])
        , testCase 25.0
            "unique \"abcabc\""
            UniqueMatcher
            "abcabc"
        , fuzzGenWith 50.0
            "unique matches reference on random small-int lists"
            smallIntListGen
            UniqueMatcher
            200
        ]

    , group MajorityPass "Exercise 3: interleave3 [MajorityPass]"
        [ testCase 25.0
            "interleave3 [1,2,3] [4,5,6] [7,8,9]"
            Interleave3Matcher
            (([1, 2, 3], [4, 5, 6], [7, 8, 9]) :: ([Int], [Int], [Int]))
        , testCase 25.0
            "interleave3 [1,2] [3] [4,5,6] (unequal lengths)"
            Interleave3Matcher
            (([1, 2], [3], [4, 5, 6]) :: ([Int], [Int], [Int]))
        , fuzz 50.0
            "interleave3 matches reference on random inputs"
            Interleave3Matcher
            (Shrink.shrink :: ([Int], [Int], [Int]) -> [([Int], [Int], [Int])])
            200
        ]

    , group AnyPass "Exercise 4: balanced [AnyPass]"
        [ testCase 25.0
            "balanced \"([]){}\""
            BalancedMatcher
            "([]){}"
        , testCase 25.0
            "balanced \"([)]\""
            BalancedMatcher
            "([)]"
        , fuzzGenWith 25.0
            "balanced matches reference on grammar-generated balanced strings"
            (grammarStringGen grammarBalanced 6 8)
            BalancedMatcher
            100
        , fuzzGenWith 25.0
            "balanced matches reference on grammar-generated bracket strings"
            (grammarStringGen grammarBrackets 8 10)
            BalancedMatcher
            100
        ]

    , group AnyPass "Exercise 5: splitOn [AnyPass]"
        [ testCase 25.0
            "splitOn ',' \"a,b,c\""
            SplitOnMatcher
            (',', "a,b,c")
        , testCase 25.0
            "splitOn ',' \",,\""
            SplitOnMatcher
            (',', ",,")
        , fuzzGenWith 50.0
            "splitOn matches reference on grammar-generated strings"
            csvInputGen
            SplitOnMatcher
            100
        ]
    ]



main :: IO ()
main = do
  _ <- setupTerminalWithChcp
  gradeAndPrint labTree

main2 :: IO ()
main2 = do
  _ <- setupTerminalWithChcp
  gradeAndPrint labTree2

-- ex 2 using composite matchers

data OutputHasDuplicates = OutputHasDuplicates

instance Matcher OutputHasDuplicates where
  describe _ _ = "produce an output that still contains duplicated elements"

instance (Eq a, Show a) => Matchable OutputHasDuplicates [a] where
  matches _ xs = hasDup (unique xs)
    where
      hasDup []     = False
      hasDup (y:ys) = y `elem` ys || hasDup ys

data KeepsAllInputElements = KeepsAllInputElements

instance Matcher KeepsAllInputElements where
  describe _ _ = "keep every element of the input"

instance (Eq a, Show a) => Matchable KeepsAllInputElements [a] where
  matches _ xs = all (`elem` unique xs) xs

data InventsNoElements = InventsNoElements

instance Matcher InventsNoElements where
  describe _ _ = "output only elements that appear in the input"

instance (Eq a, Show a) => Matchable InventsNoElements [a] where
  matches _ xs = all (`elem` xs) (unique xs)

data PreservesInputOrder = PreservesInputOrder

instance Matcher PreservesInputOrder where
  describe _ _ = "output the elements in the order they appear in the input"

instance (Eq a, Show a) => Matchable PreservesInputOrder [a] where
  matches _ xs = unique xs `isSubsequenceOf` xs

data EmptyInput = EmptyInput

instance Matcher EmptyInput where
  describe _ _ = "receive an empty input"

instance (Eq a, Show a) => Matchable EmptyInput [a] where
  matches _ xs = null xs

data EmptyOutput = EmptyOutput

instance Matcher EmptyOutput where
  describe _ _ = "produce an empty output"

instance (Eq a, Show a) => Matchable EmptyOutput [a] where
  matches _ xs = null (unique xs)


uniqueProperties :: (Eq a, Show a) => Composite [a]
uniqueProperties = AllOf
  [ NotOf (One (AnyMatcher OutputHasDuplicates))
  , One (AnyMatcher KeepsAllInputElements)
  , One (AnyMatcher InventsNoElements)
  , One (AnyMatcher PreservesInputOrder)
  , AnyOf
      [ AllOf [ One (AnyMatcher EmptyInput)
              , One (AnyMatcher EmptyOutput) ]
      , AllOf [ NotOf (One (AnyMatcher EmptyInput))
              , NotOf (One (AnyMatcher EmptyOutput)) ]
      ]
  ]

labTree3 :: TestTree
labTree3 =
  group AllPass "Exercise 2: unique (composite matchers)"
    [ testCase 20.0
        "unique [1,2,1,3,2] satisfies all unique-properties"
        (uniqueProperties :: Composite [Int])
        ([1, 2, 1, 3, 2] :: [Int])
    , testCase 20.0
        "unique \"abcabc\" satisfies all unique-properties"
        (uniqueProperties :: Composite String)
        "abcabc"
    , testCase 10.0
        "unique [] satisfies all unique-properties"
        (uniqueProperties :: Composite [Int])
        ([] :: [Int])
    , fuzzGenWith 50.0
        "random small-int lists satisfy all unique-properties"
        smallIntListGen
        (uniqueProperties :: Composite [Int])
        200
    ]

main3 :: IO ()
main3 = do
  _ <- setupTerminalWithChcp
  gradeAndPrint labTree3