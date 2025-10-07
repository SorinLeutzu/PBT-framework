module MatcherCombinators where

import Core
import Data.List (intercalate)

-- Matcher combinators

-- And combinator
data And m1 m2 a where
  And :: (Matcher m1 a, Matcher m2 a) => m1 -> m2 -> And m1 m2 a

instance (Matcher m1 a, Matcher m2 a) => Matcher (And m1 m2 a) a where
  matches (And m1 m2) x = matches m1 x && matches m2 x
  describe (And m1 m2) ok =
    let d1 = describe m1 ok
        d2 = describe m2 ok
        conJ = if ok then "and" else "nor"
     in "has both properties (" ++ d1 ++ ")" ++ conJ ++ " (" ++ d2 ++ ")"
  explainMatch (And m1 m2) x = case (firstMatch, secondMatch) of
    (False, _) -> "which fails first part " ++ explainMatch m1 x
    (_, False) -> "which fails second part " ++ explainMatch m2 x
    _ -> "which satisfies both"
    where
      firstMatch = matches m1 x
      secondMatch = matches m2 x

-- Or Combinator
data Or m1 m2 a where
  Or :: (Matcher m1 a, Matcher m2 a) => m1 -> m2 -> Or m1 m2 a

instance (Matcher m1 a, Matcher m2 a) => Matcher (Or m1 m2 a) a where
  matches (Or m1 m2) x = matches m1 x || matches m2 x
  describe (Or m1 m2) ok =
    let d1 = describe m1 ok
        d2 = describe m2 ok
     in if ok
          then "matches at least one: (" ++ d1 ++ ") or (" ++ d2 ++ ")"
          else "matches none (" ++ d1 ++ ") nor (" ++ d2 ++ ")"
  explainMatch (Or m1 m2) x
    | matches m1 x = "which satisfies first: " ++ explainMatch m1 x
    | matches m2 x = "which satisfies second" ++ explainMatch m2 x
    | otherwise = "which satisfies neither: first " ++ describe m1 False ++ "; second " ++ describe m2 False

data Not m a where
  Not :: (Matcher m a) => m -> Not m a

instance (Matcher m a) => Matcher (Not m a) a where
  matches (Not m) x = not (matches m x)
  describe (Not m) ok
    | ok = "does not" ++ d
    | otherwise = "unexpectedly" ++ d
    where
      d = describe m (not ok)
  explainMatch (Not m) x
    | matches m x = "which unexpectedly matches: " ++ describe m True
    | otherwise = "which correctly does not match: " ++ describe m False

-- All combinator
data MatcherWrapper a where
  MatcherWrapper :: (Matcher m a) => m -> MatcherWrapper a

data All a where
  All :: [MatcherWrapper a] -> All a

instance Matcher (All a) a where
  matches (All matcherList) el = all (\(MatcherWrapper m) -> matches m el) matcherList
  describe (All matcherList) ok =
    let descriptions = map (\(MatcherWrapper m) -> describe m ok) matcherList
        conj = if ok then ") and (" else ") nor ("
     in "All are satisfied:" ++ "{ " ++ intercalate conj descriptions ++ "}"

-- None combinator

assertThat :: (Matcher m a, Show a) => m -> a -> IO ()
assertThat m x
  | matches m x = return ()
  | otherwise = error $ "Assertion failed: value " ++ show x ++ " " ++ explainMatch m x

expectThat :: (Matcher m a) => m -> a -> IO (Either String ())
expectThat m x
  | matches m x = return (Right ())
  | otherwise = return (Left (explainMatch m x))