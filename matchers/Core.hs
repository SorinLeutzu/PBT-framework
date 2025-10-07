{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Core where

-- Matcher typeclass
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

-- Comparison Matchers:

-- Greater than
newtype GtMatcher a = GtMatcher a

instance (Ord a, Show a) => Matcher (GtMatcher a) a where
  matches (GtMatcher lower) higher = lower < higher
  describe (GtMatcher lower) ok
    | ok = "is greater than " ++ show lower
    | otherwise = "is not greater than " ++ show lower

-- Less than
newtype LtMatcher a = LtMatcher a

instance (Ord a, Show a) => Matcher (LtMatcher a) a where
  matches (LtMatcher higher) lower = lower < higher
  describe (LtMatcher higher) ok
    | ok = "is less than " ++ show higher
    | otherwise = "is not greater than" ++ show higher

-- Contains string matcher
newtype ContainsMatcher = ContainsMatcher String

instance Matcher ContainsMatcher String where
  matches (ContainsMatcher string) text = containsString string text
  describe (ContainsMatcher string) ok
    | ok = "Contains substring " ++ show string
    | otherwise = "does not contain " ++ show string
  explainMatch (ContainsMatcher string) text
    | matches (ContainsMatcher string) text = "which contains \"" ++ string ++ "\" in \"" ++ text ++ "\""
    | otherwise = "which does not contain \"" ++ string ++ "\" in \"" ++ text ++ "\""

-- Starts with string matcher
newtype StartsWithMatcher = StartsWithMatcher String

instance Matcher StartsWithMatcher String where
  matches (StartsWithMatcher string) text = startsWithString string text
  describe (StartsWithMatcher string) ok
    | ok = "starts with " ++ show string
    | otherwise = " does not start with " ++ show string

-- Ends with string matcher
newtype EndsWithMatcher = EndsWithMatcher String

instance Matcher EndsWithMatcher String where
  matches (EndsWithMatcher string) text = endsWithString string text
  describe (EndsWithMatcher string) ok
    | ok = "ends with " ++ show string
    | otherwise = " does not end with " ++ show string

-- Idempotence : one application does the same thing as multiple applications
data Idempotence a = Idempotence (a -> a) Int

instance (Ord a, Show a) => Matcher (Idempotence a) a where
  matches (Idempotence function times) el = oneTimeApplicationResult == nTimeApplicationResult
    where
      oneTimeApplicationResult = function el
      nTimeApplicationResult = iterate function el !! times
  describe (Idempotence f n) ok
    | ok = " Is idempotent"
    | otherwise = "Is not idempotent"

-- Invertibility: Applying an operation and its inverse is the original value
data Invertibility a b = Invertibility (a -> b) (b -> a)

instance (Ord a, Show a, Ord b, Show b) => Matcher (Invertibility a b) a where
  matches (Invertibility function inverse) el = inverse (function el) == el
  describe (Invertibility function inverse) ok
    | ok = "Is invertible "
    | otherwise = "Is not invertible "

-- Associativity
data Associativity a = Associativity (a -> a -> a)

instance (Ord a, Show a) => Matcher (Associativity a) (a, a, a) where
  matches (Associativity f) (x, y, z) = f x (f y z) == f (f x y) z
  describe (Associativity f) ok
    | ok = ""
    | otherwise = ""

-- Commutativity
-- data Commutativity a = Commutativity (a -> a -> a)
-- instance (Ord a,Show a) => Matcher (Commutativity a) (a,a) where
--   matches (Commutativity f) (x,y) = x `f` y  == y `f` x

-- -- Distributivity
-- data Distributivity a = Distributivity (a -> a -> a)
-- instance (Ord a,Show a) => Matcher (Distributivity a) (a,a,a)

-- contains elem
newtype ContainsElem a = ContainsElem a

instance (Eq a, Show a) => Matcher (ContainsElem a) [a] where
  matches (ContainsElem e) list = e `elem` list
  describe (ContainsElem e) ok
    | ok = "contains elemenent " ++ show e
    | otherwise = "does not contain element " ++ show e

eq = EqMatcher

gt = GtMatcher

contains = ContainsMatcher

-- Helpers
-- Checks if a string contains a particular string
containsString _ [] = False
containsString string text@(t : ts)
  | startsWithString string text = True
  | otherwise = containsString string ts

-- Checks if a string starts with a particular string
startsWithString [] _ = True
startsWithString _ [] = False
startsWithString string@(s : ss) text@(t : ts)
  | t == s = startsWithString ss ts
  | t /= s = False

-- checks if a string ends with a particular string
endsWithString string text = startsWithString (reverse string) (reverse text)