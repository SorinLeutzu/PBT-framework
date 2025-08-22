module Random where

import BitOps (shiftL, shiftR)
import Control.Monad
import Data.Bits (xor, (.&.))
import Data.Word qualified as W
import Next
import Sorting (sort)

newtype Rnd g a = Rnd (g -> (a, g))

type Gen a = Rnd Random a

runRandom :: (RandomGen s) => s -> Rnd s a -> a
runRandom seed (Rnd f) = fst $ f seed

evalRandom :: (RandomGen s) => s -> Rnd s a -> s
evalRandom seed (Rnd f) = snd $ f seed

execRandom :: (RandomGen s) => s -> Rnd s a -> (a, s)
execRandom seed (Rnd f) = f seed

class RandomGen a where
  seed :: a
  nextState :: a -> (W.Word64, a)

newtype Random = Random W.Word64 deriving (Show, Eq)

newtype RandomXor = RandomXor W.Word64 deriving (Show, Eq)

newtype RandomMersenne = RandomMersenne W.Word64 deriving (Show, Eq)

newtype RandomPcg = RandomPcg W.Word64 deriving (Show, Eq)

seedXor, seedMers, seedPcg, seedDefault :: W.Word64
seedXor = 0x248729837123
seedMers = 0x34347676
seedPcg = 0x84923767
seedDefault = 0x123456789ABCDEF0

instance RandomGen Random where
  seed = Random seedDefault
  nextState (Random s) =
    let w' = nextXor s
     in (w', Random w')

instance RandomGen RandomXor where
  seed = RandomXor seedXor
  nextState (RandomXor s) =
    let w' = nextXor s
     in (w', RandomXor w')

instance RandomGen RandomMersenne where
  seed = RandomMersenne seedMers
  nextState (RandomMersenne s) =
    let w' = nextMersenne s
     in (w', RandomMersenne w')

instance RandomGen RandomPcg where
  seed = RandomPcg seedPcg
  nextState (RandomPcg s) =
    let w' = nextpcg64 s
     in (w', RandomPcg w')

instance Functor (Rnd g) where
  fmap f (Rnd a) = Rnd $ \s ->
    let (v, s') = a s
     in (f v, s')

instance Applicative (Rnd g) where
  pure a = Rnd $ \s -> (a, s)
  (Rnd f) <*> (Rnd v) = Rnd $ \s ->
    let (g, s') = f s
        (x, s'') = v s'
     in (g x, s'')

instance Monad (Rnd g) where
  return = pure
  (Rnd a) >>= f = Rnd $ \s ->
    let (b, s') = a s
        Rnd g = f b
     in g s'

nextDouble :: (RandomGen g) => Rnd g Double
nextDouble = Rnd $ \g ->
  let (w, g') = nextState g
      maxW = fromIntegral (maxBound :: W.Word64) :: Double
   in (fromIntegral w / maxW, g')

nextDoubleRange :: (RandomGen g) => Double -> Double -> Rnd g Double
nextDoubleRange lo hi = do
  d <- nextDouble
  return $ lo + (hi - lo) * d

nextInt :: (RandomGen g) => Rnd g Int
nextInt = Rnd $ \g ->
  let (w, g') = nextState g
      maxI = fromIntegral (maxBound :: Int) :: Integer
      n = (fromIntegral w :: Integer) `mod` (maxI + 1)
   in (fromIntegral n, g')

nextIntRange :: (RandomGen g) => Int -> Int -> Rnd g Int
nextIntRange lo hi
  | hi <= lo = return lo
  | otherwise = do
      n <- nextInt
      let range = hi - lo
       in return (lo + (n `mod` range))

nextBool :: (RandomGen g) => Rnd g Bool
nextBool = Rnd $ \g ->
  let (w, g') = nextState g
   in ((w .&. 1) == 1, g')

nextPositiveInt :: (RandomGen g) => Rnd g Int
nextPositiveInt = nextIntRange 0 maxBound

randomSample :: (RandomGen g) => a -> [a] -> Rnd g a
randomSample def [] = return def
randomSample def as = do
  idx <- nextIntRange 0 (length as)
  return $ as !! idx

class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Int where
  arbitrary = nextInt

instance Arbitrary Bool where
  arbitrary = nextBool

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
