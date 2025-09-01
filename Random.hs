{-# LANGUAGE LambdaCase #-}

module Random where

import BitOps (shiftL, shiftR)
import Control.Monad ()
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    execState,
    runState,
  )
import Data.Bits (xor, (.&.))
import Data.Char ()
import Data.List (nub)
import Data.Word qualified as W
import Next (nextMersenne, nextXor, nextpcg64)
import Sorting (sort)

data PRNG
  = PRNG_Xor W.Word64
  | PRNG_Mersenne W.Word64
  | PRNG_Pcg W.Word64
  deriving (Show, Eq)

seedXor, seedMersenne, seedPcg :: W.Word64
seedXor = 0x248729837123
seedMersenne = 0x34347676
seedPcg = 0x84923767

nextPRNG :: PRNG -> (W.Word64, PRNG)
nextPRNG = \case
  PRNG_Xor s ->
    let w' = nextXor s
     in (w', PRNG_Xor w')
  PRNG_Mersenne s ->
    let w' = nextMersenne s
     in (w', PRNG_Mersenne w')
  PRNG_Pcg s ->
    let w' = nextpcg64 s
     in (w', PRNG_Pcg w')

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
      let range = hi - lo
       in pure (lo + (n `mod` range))

nextBool :: Rnd Bool
nextBool = do
  w <- nextWord64
  pure ((w .&. 1) == 1)

nextPositiveInt :: Rnd Int
nextPositiveInt = nextIntRange 0 maxBound

randomSample :: a -> [a] -> Rnd a
randomSample def [] = pure def
randomSample def as = do
  idx <- nextIntRange 0 (length as)
  pure (as !! idx)

class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
  shrink _ = []

instance Arbitrary Int where
  arbitrary = nextInt
  shrink = shrinkIntStd

instance Arbitrary Bool where
  arbitrary = nextBool
  shrink = shrinkBool

instance (Arbitrary a) => Arbitrary [a] where
  arbitrary = do
    i <- nextIntRange 0 4
    if i > 0
      then (:) <$> arbitrary <*> arbitrary
      else return []

instance Arbitrary Char where
  arbitrary = do
    let allChars = enumFromTo (minBound :: Char) (maxBound :: Char)
    randomSample (head allChars) allChars

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = (,) <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c) where
  arbitrary = (,,) <$> arbitrary <*> arbitrary <*> arbitrary

newtype SmallInt = SmallInt {getSmallInt :: Int} deriving (Eq, Ord)

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> nextIntRange 0 100

instance Show SmallInt where
  show (SmallInt i) = show i

newtype PrintableChar = PrintableChar {getPrintableChar :: Char} deriving (Eq)

instance Arbitrary PrintableChar where
  arbitrary = PrintableChar <$> randomSample 'a' (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " !@#$%^&*()_+-=[]{}")

newtype PrintableString = PrintableString {getPrintableString :: String} deriving (Eq)

instance Arbitrary PrintableString where
  arbitrary = do
    printableChrs <- (arbitrary :: Gen [PrintableChar])
    return $ PrintableString (map getPrintableChar printableChrs)

instance Show PrintableString where
  show (PrintableString s) = show s

newtype AlphaNumChar = AlphaNumChar {getAlphaNumChar :: Char} deriving (Eq)

instance Arbitrary AlphaNumChar where
  arbitrary = AlphaNumChar <$> randomSample 'a' (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

newtype AlphaNumString = AlphaNumString {getAlphaNumString :: String} deriving (Eq)

instance Arbitrary AlphaNumString where
  arbitrary = do
    printableChrs <- (arbitrary :: Gen [AlphaNumChar])
    return $ AlphaNumString (map getAlphaNumChar printableChrs)

instance Show AlphaNumString where
  show (AlphaNumString s) = show s

newtype SortedList a = SortedList {getSortedList :: [a]} deriving (Eq, Ord)

instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
  arbitrary = SortedList . sort <$> (arbitrary :: Gen [a])

instance (Show a) => Show (SortedList a) where
  show (SortedList l) = show l

-- works by halving the parameter value consecutevely
shrinkIntStd :: Int -> [Int]
shrinkIntStd 0 = []
shrinkIntStd x = takeWhile (/= 0) $ iterate (`div` 2) x

sgn x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

log2 :: Int -> Int
log2 x | x >= 0 && x < 2 = 0
log2 x
  | x >= 2 = exp2 1 0 x
  | x < 0 = sgn x * exp2 1 0 (x `div` sgn x)

exp2 :: Int -> Int -> Int -> Int
exp2 acc power upperBound
  | acc < upperBound = exp2 (acc * 2) (power + 1) upperBound
  | otherwise = power

-- works by taking the base 2 logarithm consecutively
shrinkIntAgg :: Int -> [Int]
shrinkIntAgg 0 = []
shrinkIntAgg x = takeWhile (/= 0) $ iterate log2 x

shrinkBool :: Bool -> [Bool]
shrinkBool True = [False]
shrinkBool False = []

-- shrinks lists by removing power of 2 elements
shrinkListStd :: (a -> [a]) -> [a] -> Int -> [[a]]
shrinkListStd shrinkingFunction list acc
  | acc > length list = []
  | otherwise =
      let remainingList = drop acc list
          shrinkedRemainingList = map (!! 1) (map shrinkingFunction remainingList)
       in remainingList : shrinkedRemainingList : shrinkListStd shrinkingFunction shrinkedRemainingList (acc * 2)

-- t = take 8 (shrinkListStd shrinkIntStd [200, 300, 400, 500, 600, 700] 1)

-- shrinks list by halving the list until the list is 10 elements or less
-- then generates all combinations and chooses the shortest
-- shrinkListAgg :: (a -> [a]) -> [a] -> [[a]]
-- shrinkListAgg shrinkingFunction list = reduceSize shrinkingFunction list ++ (orderedSublists shrinkingFunction [list])

reduceSize :: (a -> [a]) -> [a] -> [[a]]
reduceSize shrinkingFunction list =
  let halvedListLen = length list `div` 2
      remainingList = take halvedListLen list
      shrinkedRemainingList = map (!! 1) (map shrinkingFunction remainingList)
   in if (length list > 2)
        then remainingList : shrinkedRemainingList : reduceSize shrinkingFunction shrinkedRemainingList
        else [list]

dropOneElLeftAndRight :: [a] -> [[a]]
dropOneElLeftAndRight list = init list : [tail list]

applyDropOneElLeftAndRight :: [[a]] -> [[a]]
applyDropOneElLeftAndRight list = concat (map dropOneElLeftAndRight list)

orderedSublists :: (Eq a) => [[a]] -> [[a]]
orderedSublists list
  | length (last list) == 1 = list
  | otherwise =
      let subListsOfSameLength = applyDropOneElLeftAndRight list
          shorterSublists = orderedSublists subListsOfSameLength
       in nub (subListsOfSameLength ++ shorterSublists)