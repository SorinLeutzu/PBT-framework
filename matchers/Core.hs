module Matchers.Core where

import Data.Typeable (Typeable, typeOf)
import Data.List (intercalate)


-- A matcher that can describe itself independently of the value it matches.
class Matcher m where
  describe :: m -> Bool -> String

-- A value that can be checked by a particular matcher.
class Matcher m => Matchable m a where
  matches :: m -> a -> Bool
  explainMatch :: Matchable m a => m -> a -> String
  explainMatch m x =
    let ok = matches m x
        desc = describe m ok
    in if ok
        then "which " ++ desc
        else "which does not " ++ desc


data AnyMatcher a where
  AnyMatcher :: (Matchable m a) => m -> AnyMatcher a

data Composite a
  = One (AnyMatcher a)
  | AllOf [Composite a]
  | AnyOf [Composite a]
  | NotOf (Composite a)

instance Matcher (Composite a) where
  describe _ ok =
    if ok
      then "satisfies composite matcher"
      else "does not satisfy composite matcher"

instance Matchable (Composite a) a where
  matches (One (AnyMatcher m)) x = matches m x
  matches (AllOf ms) x = all (`matches` x) ms
  matches (AnyOf ms) x = any (`matches` x) ms
  matches (NotOf m) x = not (matches m x)

  explainMatch m x =
    snd (evalComposite m x)
    where
      leafExplain :: (Matchable mm a) => mm -> Bool -> String
      leafExplain mm ok =
        let d = describe mm ok
         in if ok then "which " ++ d else "which does not " ++ d

      evalAny :: AnyMatcher a -> a -> (Bool, String)
      evalAny (AnyMatcher inner) v =
        let ok = matches inner v
         in (ok, leafExplain inner ok)

      evalComposite :: Composite a -> a -> (Bool, String)
      evalComposite (One am) v = evalAny am v
      evalComposite (NotOf inner) v =
        let (okInner, msgInner) = evalComposite inner v
            ok = not okInner
         in if ok
              then (True, "which correctly does not match inner composite")
              else (False, "which unexpectedly matches inner composite: " ++ msgInner)
      evalComposite (AllOf ms) v =
        let rs = map (\cm -> evalComposite cm v) ms
         in case filter (not . fst) rs of
              [] -> (True, "which satisfies all parts of the composite")
              ((_, msg) : _) -> (False, "which fails one part of the composite: " ++ msg)
      evalComposite (AnyOf ms) v =
        let rs = map (\cm -> evalComposite cm v) ms
         in case filter fst rs of
              ((_, msg) : _) -> (True, "which satisfies one part of the composite: " ++ msg)
              [] ->
                ( False,
                  "which satisfies none of the composite parts: "
                    ++ intercalate "; " (map snd rs)
                )


data AnyMatch where
  AnyMatch :: (Matchable m a) => m -> a -> AnyMatch

data MultiComposite
  = OneM AnyMatch
  | AllOfM [MultiComposite]
  | AnyOfM [MultiComposite]
  | NotOfM MultiComposite

instance Matcher MultiComposite where
  describe _ ok =
    if ok
      then "satisfies multi composite matcher"
      else "does not satisfy multi composite matcher"

instance Matchable MultiComposite () where
  matches mc () = fst (evalMultiComposite mc)

  explainMatch mc () = snd (evalMultiComposite mc)

evalAnyMatch :: AnyMatch -> (Bool, String)
evalAnyMatch (AnyMatch m v) =
  let ok = matches m v
      d = describe m ok
      msg = if ok then "which " ++ d else "which does not " ++ d
   in (ok, msg)

evalMultiComposite :: MultiComposite -> (Bool, String)
evalMultiComposite (OneM am) = evalAnyMatch am
evalMultiComposite (NotOfM inner) =
  let (okInner, msgInner) = evalMultiComposite inner
      ok = not okInner
   in if ok
        then (True, "which correctly does not match inner multi composite")
        else (False, "which unexpectedly matches inner multi composite: " ++ msgInner)
evalMultiComposite (AllOfM ms) =
  let rs = map evalMultiComposite ms
   in case filter (not . fst) rs of
        [] -> (True, "which satisfies all parts of the multi composite")
        ((_, msg) : _) -> (False, "which fails one part of the multi composite: " ++ msg)
evalMultiComposite (AnyOfM ms) =
  let rs = map evalMultiComposite ms
   in case filter fst rs of
        ((_, msg) : _) -> (True, "which satisfies one part of the multi composite: " ++ msg)
        [] ->
          ( False,
            "which satisfies none of the multi composite parts: "
              ++ intercalate "; " (map snd rs)
          )

-- Basic matchers:

-- Equality matcher
newtype EqMatcher a = EqMatcher a

instance (Eq a, Show a) => Matcher (EqMatcher a) where
  describe (EqMatcher act) ok
    | ok = "is equal to " ++ show act
    | otherwise = "is not equal to " ++ show act

instance (Eq a, Show a) => Matchable (EqMatcher a) a where
  matches (EqMatcher exp) act = exp == act

-- Greater than
newtype GtMatcher a = GtMatcher a

instance (Ord a, Show a) => Matcher (GtMatcher a) where
  describe (GtMatcher lower) ok
    | ok = "is greater than " ++ show lower
    | otherwise = "is not greater than " ++ show lower

instance (Ord a, Show a) => Matchable (GtMatcher a) a where
  matches (GtMatcher lower) higher = lower < higher

-- Less than
newtype LtMatcher a = LtMatcher a

instance (Ord a, Show a) => Matcher (LtMatcher a) where
  describe (LtMatcher higher) ok
    | ok = "is less than " ++ show higher
    | otherwise = "is not greater than" ++ show higher

instance (Ord a, Show a) => Matchable (LtMatcher a) a where
  matches (LtMatcher higher) lower = lower < higher

-- Contains string matcher
newtype ContainsMatcher = ContainsMatcher String

instance Matcher ContainsMatcher where
  describe (ContainsMatcher string) ok
    | ok = "Contains substring " ++ show string
    | otherwise = "does not contain " ++ show string

instance Matchable ContainsMatcher String where
  matches (ContainsMatcher string) text = containsString string text

-- Starts with string matcher
newtype StartsWithMatcher = StartsWithMatcher String

instance Matcher StartsWithMatcher where
  describe (StartsWithMatcher string) ok
    | ok = "starts with " ++ show string
    | otherwise = " does not start with " ++ show string

instance Matchable StartsWithMatcher String where
  matches (StartsWithMatcher string) text = startsWithString string text

-- Ends with string matcher
newtype EndsWithMatcher = EndsWithMatcher String

instance Matcher EndsWithMatcher where
  describe (EndsWithMatcher string) ok
    | ok = "ends with " ++ show string
    | otherwise = " does not end with " ++ show string

instance Matchable EndsWithMatcher String where
  matches (EndsWithMatcher string) text = endsWithString string text

-- Idempotence : one application does the same thing as multiple applications
data Idempotence a = Idempotence (a -> a) Int

instance (Ord a, Show a) => Matcher (Idempotence a) where
  describe (Idempotence _ n) ok
    | ok = " Is idempotent on " ++ show n ++ " applications "
    | otherwise = "Is not idempotent"

instance (Ord a, Show a) => Matchable (Idempotence a) a where
  matches (Idempotence function times) el = oneTimeApplicationResult == nTimeApplicationResult
    where
      oneTimeApplicationResult = function el
      nTimeApplicationResult = iterate function el !! times

-- Invertibility: Applying an operation and its inverse is the original value
data Invertibility a b = Invertibility (a -> b) (b -> a)

instance (Ord a, Show a, Ord b, Show b) => Matcher (Invertibility a b) where
  describe _ ok
    | ok = "Is invertible "
    | otherwise = "Is not invertible "

instance (Ord a, Show a, Ord b, Show b) => Matchable (Invertibility a b) a where
  matches (Invertibility function inverse) el = inverse (function el) == el

-- Associativity
data Associativity a = Associativity (a -> a -> a)

instance (Ord a, Show a, Typeable a) => Matcher (Associativity a) where
  describe (Associativity f) ok
    | ok = "Function " ++ show (typeOf f) ++ "is associative on "
    | otherwise = "Function " ++ show (typeOf f) ++ "is NOT associative on "

instance (Ord a, Show a, Typeable a) => Matchable (Associativity a) (a, a, a) where
  matches (Associativity f) (x, y, z) = f x (f y z) == f (f x y) z

-- Commutativity
data Commutativity a = Commutativity (a -> a -> a)

instance (Ord a, Show a, Typeable a) => Matcher (Commutativity a) where
  describe (Commutativity f) ok
    | ok = "Function " ++ show (typeOf f) ++ " is commutative on"
    | otherwise = "Function " ++ show (typeOf f) ++ " is NOT commutative on"

instance (Ord a, Show a, Typeable a) => Matchable (Commutativity a) (a, a) where
  matches (Commutativity f) (x, y) = x `f` y == y `f` x

-- Distributivity
data Distributivity a = Distributivity (a -> a -> a) (a -> a -> a)

instance (Ord a, Show a, Typeable a) => Matcher (Distributivity a) where
  describe (Distributivity _ times) ok
    | ok = "Operation " ++ show (typeOf times) ++ " is distributive on operation " ++ show (typeOf times) ++ " on domain "
    | otherwise = "Operation " ++ show (typeOf times) ++ " is NOT distributive on operation " ++ show (typeOf times) ++ " on domain "

instance (Ord a, Show a, Typeable a) => Matchable (Distributivity a) (a, a, a) where
  matches (Distributivity plus times) (x, y, z) =
    x `times` (y `plus` z) == (x `times` y) `plus` (x `times` z) &&
    (y `plus` z) `times` x == (y `times` x) `plus` (z `times` x)

-- contains elem
newtype ContainsElem a = ContainsElem a

instance (Eq a, Show a) => Matcher (ContainsElem a) where
  describe (ContainsElem e) ok
    | ok = "contains elemenent " ++ show e
    | otherwise = "does not contain element " ++ show e

instance (Eq a, Show a) => Matchable (ContainsElem a) [a] where
  matches (ContainsElem e) list = e `elem` list


-- Smart constructors
eq :: a -> EqMatcher a
eq = EqMatcher

gt :: a -> GtMatcher a
gt = GtMatcher

contains :: String -> ContainsMatcher
contains = ContainsMatcher

-- Helpers
-- Checks if a string contains a particular string
containsString :: String -> String -> Bool
containsString _ [] = False
containsString string text@(_ : ts)
  | startsWithString string text = True
  | otherwise = containsString string ts

-- Checks if a string starts with a particular string
startsWithString :: String -> String -> Bool
startsWithString [] _ = True
startsWithString _ [] = False
startsWithString string@(s : ss) text@(t : ts)
  | t == s = startsWithString ss ts
  | t /= s = False

-- checks if a string ends with a particular string
endsWithString :: String -> String -> Bool
endsWithString string text = startsWithString (reverse string) (reverse text)