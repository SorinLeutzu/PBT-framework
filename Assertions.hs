{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Assertions
  ( Matcher (..),
    EqMatcher (..),
    GtMatcher (..),
    ContainsMatcher (..),
    And (..),
    Or (..),
    Not (..),
    eq,
    gt,
    contains,
    assertThat,
    expectThat,
  )
where

-- Checks if a string contains a particular string
containsString _ [] = False
containsString string text@(t : ts) = if (startsWithString string text) then True else (containsString string ts)

-- Checks if a string starts with a particular string
startsWithString [] _ = True
startsWithString _ [] = False
startsWithString string@(s : ss) text@(t : ts)
  | t == s = startsWithString ss ts
  | t /= s = False

-- Mathcer typeclass
class Matcher m a | m -> a where
  matches :: m -> a -> Bool
  describe :: m -> Bool -> String

  explainMatch :: m -> a -> String
  explainMatch m x =
    let ok = matches m x
        desc = describe m ok
     in if ok
          then "which " ++ desc
          else "which does not " ++ desc

-- Basic matchers:

-- Equality matcher
newtype EqMatcher a = EqMatcher a

instance (Eq a, Show a) => Matcher (EqMatcher a) a where
  matches (EqMatcher exp) act = exp == act
  describe :: (Eq a, Show a) => EqMatcher a -> Bool -> String
  describe (EqMatcher act) ok
    | ok = "is equal to " ++ show act
    | otherwise = "is not equal to " ++ show act

-- Comparison Mathcer
newtype GtMatcher a = GtMatcher a

instance (Ord a, Show a) => Matcher (GtMatcher a) a where
  matches (GtMatcher higher) lower = higher > lower
  describe (GtMatcher higher) ok
    | ok = "is greater than " ++ show higher
    | otherwise = "id not greater than " ++ show higher

-- Contains matcher
newtype ContainsMatcher = ContainsMatcher String

instance Matcher ContainsMatcher String where
  matches (ContainsMatcher string) text = containsString string text
  describe (ContainsMatcher string) ok
    | ok = "Contains substring " ++ show string
    | otherwise = "does not contain " ++ show string
  explainMatch (ContainsMatcher string) text
    | matches (ContainsMatcher string) text = "which contains \"" ++ string ++ "\" in \"" ++ text ++ "\""
    | otherwise = "which does not contain \"" ++ string ++ "\" in \"" ++ text ++ "\""

eq = EqMatcher

gt = GtMatcher

contains = ContainsMatcher

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

assertThat :: (Matcher m a, Show a) => m -> a -> IO ()
assertThat m x
  | matches m x = return ()
  | otherwise = error $ "Assertion failed: value " ++ show x ++ " " ++ explainMatch m x

expectThat :: (Matcher m a) => m -> a -> IO (Either String ())
expectThat m x
  | matches m x = return (Right ())
  | otherwise = return (Left (explainMatch m x))