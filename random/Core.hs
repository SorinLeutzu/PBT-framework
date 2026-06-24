{-# LANGUAGE LambdaCase #-}


module Random.Core where

import Control.Monad (guard)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    execState,
    runState,
  )
import Data.Bits (xor, (.&.))
import Data.List (sort)
import Data.Word qualified as W
import Random.PRNG (nextMersenne, nextXor, nextpcg64)

import Data.Map qualified as M


data CharOpt = CharAny | CharPrintable | CharAlphaNum deriving (Eq, Show)

data StringOpt = StringAny | StringPrintable | StringAlphaNum deriving (Eq, Show)

data IntOpt = IntAny | IntRange Int Int deriving (Eq, Show)

data ListOrder = ListRandom | ListAscending | ListDescending deriving (Eq, Show)

data ListOpt o = ListOpt
  { listMinLen :: Int,
    listMaxLen :: Int,
    listOrder :: ListOrder,
    listElemOpt :: o
  }
  deriving (Eq, Show)

defaultListOpt :: o -> ListOpt o
defaultListOpt o = ListOpt
    { listMinLen = 0,
      listMaxLen = 4,
      listOrder = ListRandom,
      listElemOpt = o
    }



data PRNG = PRNG_Xor W.Word64 | PRNG_Mersenne W.Word64| PRNG_Pcg W.Word64
  deriving (Show, Eq)

seedXor, seedMersenne, seedPcg :: W.Word64
seedXor = 0x248729837123
seedMersenne = 5489
seedPcg = 0x853c49e6748fea9b

nextPRNG :: PRNG -> (W.Word64, PRNG)
nextPRNG = \case
  PRNG_Xor s ->
    let w' = nextXor s
     in (w', PRNG_Xor w')
  PRNG_Mersenne s ->
    let w' = nextMersenne s
     in (w', PRNG_Mersenne w')
  PRNG_Pcg s ->
    let (out, s') = nextpcg64 s
     in (out, PRNG_Pcg s')

type Rnd a = State PRNG a

type Gen a = Rnd a

runRandom :: PRNG -> Rnd a -> a
runRandom = flip evalState

evalRandom :: PRNG -> Rnd a -> PRNG
evalRandom = flip execState

execRandom :: PRNG -> Rnd a -> (a, PRNG)
execRandom = flip runState

nextWord64 :: Rnd W.Word64
nextWord64 = do
  prng <- get
  let (w, prng') = nextPRNG prng
  put prng'
  pure w

nextDouble :: Rnd Double
nextDouble = do
  w <- nextWord64
  let maxW = fromIntegral (maxBound :: W.Word64) :: Double
  pure (fromIntegral w / maxW)

nextDoubleRange :: Double -> Double -> Rnd Double
nextDoubleRange lo hi = do
  d <- nextDouble
  pure (lo + (hi - lo) * d)

nextInt :: Rnd Int
nextInt = do
  w <- nextWord64
  let maxI = fromIntegral (maxBound :: Int) :: Integer
      n = (fromIntegral w :: Integer) `mod` (maxI + 1)
  pure (fromIntegral n)

nextIntRange :: Int -> Int -> Rnd Int
nextIntRange lo hi
  | hi <= lo = pure lo
  | otherwise = do
      n <- nextInt
      let range = hi - lo + 1
       in pure (lo + (n `mod` range))

nextBool :: Rnd Bool
nextBool = do
  w <- nextWord64
  pure ((w .&. 1) == 1)

nextPositiveInt :: Rnd Int
nextPositiveInt = nextInt

randomSample :: a -> [a] -> Rnd a
randomSample def [] = pure def
randomSample def as = do
  idx <- nextIntRange 0 (length as)
  pure (as !! idx)

randomElement :: [a] -> Rnd a
--randomElement [] = error "randomElement: empty list"
randomElement xs = do
    idx <- nextIntRange 0 (length xs - 1)
    return (xs !! idx)

class Arbitrary a o where
  arbitrary :: o -> Gen a

instance Arbitrary Int () where
  arbitrary () = nextInt

instance Arbitrary Int IntOpt where
  arbitrary opt =
    case opt of
      IntAny -> nextInt
      IntRange lo hi -> nextIntRange lo hi

instance Arbitrary Bool () where
  arbitrary () = nextBool

instance (Arbitrary a ()) => Arbitrary [a] () where
  arbitrary () = do
    n <- nextIntRange 0 4
    sequenceA (replicate n (arbitrary ()))


instance (Arbitrary a o, Ord a) => Arbitrary [a] (ListOpt o) where
  arbitrary opt = do
    let lo = max 0 (listMinLen opt)
        hiRaw = max 0 (listMaxLen opt)
        hi = if hiRaw < lo then lo else hiRaw
    n <- nextIntRange lo hi
    xs <- sequenceA (replicate n (arbitrary (listElemOpt opt)))
    pure $
      case listOrder opt of
        ListRandom -> xs
        ListAscending -> sort xs
        ListDescending -> reverse (sort xs)

instance Arbitrary Char () where
  arbitrary () = do
    let allChars = enumFromTo (minBound :: Char) (maxBound :: Char)
    randomSample (head allChars) allChars

instance Arbitrary Char CharOpt where
  arbitrary opt =
    case opt of
      CharAny -> arbitrary ()
      CharPrintable -> randomSample 'a' printableChars
      CharAlphaNum -> randomSample 'a' alphaNumChars


arbitraryString :: StringOpt -> Gen String
arbitraryString opt =
  case opt of
    StringAny -> (arbitrary () :: Gen [Char])
    StringPrintable -> do
      n <- nextIntRange 0 4
      sequenceA (replicate n (arbitrary CharPrintable))
    StringAlphaNum -> do
      n <- nextIntRange 0 4
      sequenceA (replicate n (arbitrary CharAlphaNum))

instance (Arbitrary a (), Arbitrary b ()) => Arbitrary (a, b) () where
  arbitrary () = (,) <$> arbitrary () <*> arbitrary ()

instance (Arbitrary a oa, Arbitrary b ob) => Arbitrary (a, b) (oa,ob) where
  arbitrary (oa, ob) = (,) <$> arbitrary oa <*> arbitrary ob

instance (Arbitrary a (), Arbitrary b (), Arbitrary c ()) => Arbitrary (a, b, c) () where
  arbitrary () = (,,) <$> arbitrary () <*> arbitrary () <*> arbitrary ()

instance (Arbitrary a oa, Arbitrary b ob, Arbitrary c oc) => Arbitrary (a, b, c) (oa,ob,oc) where
  arbitrary (oa, ob, oc) = (,,) <$> arbitrary oa <*> arbitrary ob <*> arbitrary oc

lowercaseLetters = ['a' .. 'z']

uppercaseLetters = ['A' .. 'Z']

digits = ['0' .. '9']

specialPrintableCharacters = " !@#$%^&*()_+-=[]{}"

printableChars = lowercaseLetters ++ uppercaseLetters ++ digits ++ specialPrintableCharacters

alphaNumChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']


newtype SortedList a = SortedList {getSortedList :: [a]} deriving (Eq, Ord)

instance (Arbitrary a o, Ord a) => Arbitrary (SortedList a) o where
  arbitrary o =
    let opt = defaultListOpt o
     in SortedList . sort <$> (arbitrary opt :: Gen [a])

instance (Show a) => Show (SortedList a) where
  show (SortedList l) = show l

newtype GrammarFuzzedString = FuzzedString {getFuzzedString:: String}
