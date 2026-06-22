module Matchers.Combinators where

import Matchers.Core

-- Matcher combinators

-- And combinator
data And a where
  And :: (Matchable m1 a, Matchable m2 a) => m1 -> m2 -> And a

instance Matcher (And a) where
  describe _ ok =
    if ok then "satisfies both parts" else "does not satisfy both parts"

instance Matchable (And a) a where
  matches (And m1 m2) x = matches m1 x && matches m2 x
  explainMatch (And m1 m2) x =
    let r1 = matches m1 x
        r2 = matches m2 x
        leafExplain :: (Matchable m a) => m -> Bool -> String
        leafExplain m ok =
          let d = describe m ok
           in if ok then "which " ++ d else "which does not " ++ d
     in case (r1, r2) of
          (False, _) -> "which fails first part: " ++ leafExplain m1 r1
          (_, False) -> "which fails second part: " ++ leafExplain m2 r2
          _ -> "which satisfies both parts"

-- Or Combinator
data Or a where
  Or :: (Matchable m1 a, Matchable m2 a) => m1 -> m2 -> Or a

instance Matcher (Or a) where
  describe _ ok =
    if ok then "satisfies at least one part" else "satisfies neither part"

instance Matchable (Or a) a where
  matches (Or m1 m2) x = matches m1 x || matches m2 x
  explainMatch (Or m1 m2) x =
    let r1 = matches m1 x
        r2 = matches m2 x
        leafExplain :: (Matchable m a) => m -> Bool -> String
        leafExplain m ok =
          let d = describe m ok
           in if ok then "which " ++ d else "which does not " ++ d
     in if r1
          then "which satisfies first part: " ++ leafExplain m1 r1
          else
            if r2
              then "which satisfies second part: " ++ leafExplain m2 r2
              else
                "which satisfies neither part; first: "
                  ++ leafExplain m1 r1
                  ++ "; second: "
                  ++ leafExplain m2 r2

data Not a where
  Not :: (Matchable m a) => m -> Not a

instance Matcher (Not a) where
  describe _ ok =
    if ok then "satisfies negation" else "does not satisfy negation"

instance Matchable (Not a) a where
  matches (Not m) x = not (matches m x)
  explainMatch (Not m) x =
    let r = matches m x
        leafExplain :: (Matchable mm a) => mm -> Bool -> String
        leafExplain mm ok =
          let d = describe mm ok
           in if ok then "which " ++ d else "which does not " ++ d
     in if r
          then "which unexpectedly matches: " ++ leafExplain m r
          else "which correctly does not match: " ++ leafExplain m r

-- All combinator
data MatcherWrapper a where
  MatcherWrapper :: (Matchable m a) => m -> MatcherWrapper a

data All a where
  All :: [MatcherWrapper a] -> All a

instance Matcher (All a) where
  describe _ ok =
    if ok then "satisfies all parts" else "does not satisfy all parts"

instance Matchable (All a) a where
  matches (All matcherList) el = all (\(MatcherWrapper m) -> matches m el) matcherList
  explainMatch (All matcherList) x =
    case firstFailure matcherList of
      Nothing -> "which satisfies all parts"
      Just (MatcherWrapper m, ok) -> "which fails: " ++ leafExplain m ok
    where
      leafExplain :: (Matchable m a) => m -> Bool -> String
      leafExplain m ok =
        let d = describe m ok
         in if ok then "which " ++ d else "which does not " ++ d
      firstFailure :: [MatcherWrapper a] -> Maybe (MatcherWrapper a, Bool)
      firstFailure [] = Nothing
      firstFailure (mw@(MatcherWrapper m) : rest) =
        let ok = matches m x
         in if ok then firstFailure rest else Just (mw, ok)

-- None combinator

assertThat :: (Matchable m a, Show a) => m -> a -> IO ()
assertThat m x
  | matches m x = return ()
  | otherwise = error $ "Assertion failed: value " ++ show x ++ " " ++ explainMatch m x

expectThat :: (Matchable m a) => m -> a -> IO (Either String ())
expectThat m x
  | matches m x = return (Right ())
  | otherwise = return (Left (explainMatch m x))