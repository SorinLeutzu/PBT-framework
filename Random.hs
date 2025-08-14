module Random where

import BitOps (shiftL, shiftR)
import Control.Monad
import Data.Bits (Bits (xor))
import Data.Word qualified as W
import Next
import Sorting (sort)

newtype Random = Random W.Word64 deriving (Show)

newtype Rnd g a = Rnd (g -> (a, g))

type Gen a = Rnd Random a

runRandom :: (RandomSeed s) => s -> Rnd s a -> a
runRandom seed (Rnd f) = fst $ f seed

evalRandom :: (RandomSeed s) => s -> Rnd s a -> s
evalRandom seed (Rnd f) = snd $ f seed

execRandom :: (RandomSeed s) => s -> Rnd s a -> (a, s)
execRandom seed (Rnd f) = f seed

varyRandom :: Random -> Int -> Random
varyRandom (Random r) v = Random $ (iterate nextState r) !! v

class RandomSeed a where
  newSeed :: a

instance RandomSeed Random where
  newSeed = newRandom 0x248729837123

instance Functor (Rnd g) where
  fmap f (Rnd a) = Rnd $ \s ->
    let (v, seed) = a s
     in (f v, seed)

instance (RandomSeed g) => Applicative (Rnd g) where
  pure a = Rnd $ \s -> (a, s)
  (Rnd f) <*> (Rnd v) = Rnd $ \s ->
    let (g, seed) = f s
        (x, seed') = v seed
     in (g x, seed')

instance (RandomSeed g) => Monad (Rnd g) where
  return a = Rnd $ \s -> (a, s)
  (Rnd a) >>= f = Rnd $ \s ->
    let (b, seed) = a s
        Rnd g = f b
     in g seed

newRandom :: W.Word64 -> Random
newRandom seed = Random seed

nextDouble :: Rnd Random Double
nextDouble = Rnd nd
  where
    maxW = maxBound :: W.Word64
    nd (Random w) =
      (fromIntegral w / fromIntegral maxW, Random $ nextState w)

nextDoubleRange :: Double -> Double -> Rnd Random Double
nextDoubleRange lo hi = do
  d <- nextDouble
  return $ lo + (hi - lo) * d

nextInt :: Rnd Random Int
nextInt = Rnd ni
  where
    maxW = maxBound :: W.Word64
    maxI = maxBound :: Int
    ni (Random w) =
      (fromIntegral w `div` ((fromIntegral maxW) `div` maxI), Random $ nextState w)

nextIntRange :: Int -> Int -> Rnd Random Int
nextIntRange lo hi = do
  n <- nextInt
  return (lo + n `mod` (hi - lo))

nextBool :: Rnd Random Bool
nextBool = Rnd ni
  where
    ni (Random w) =
      ((fromIntegral w :: Int) `div` 2 == 0, Random $ nextState w)

nextPositiveInt :: Rnd Random Int
nextPositiveInt = nextIntRange 0 maxBound

randomSample :: a -> [a] -> Rnd Random a
randomSample a [] = return a
randomSample a as = do
  let l = length as + 1
  idx <- nextIntRange 0 l
  return $ (a : as) !! idx

class Arbitrary a where
  arbitrary :: Rnd Random a

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
    randomSample minBound (enumFromTo (succ minBound) maxBound)

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = liftM2 (,) arbitrary arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c) where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary

-- >>> runRandom newSeed nextInt
-- -40162935664931

-- >>> runRandom newSeed (arbitrary :: Rnd Random (Bool, Bool))
-- (False,False)

-- >>> runRandom newSeed (arbitrary :: Rnd Random (Bool, Int, Int))
-- (True,7319755751108929560,-666014613197512405)

-- >>> runRandom newSeed (arbitrary :: Rnd Random String)
-- "\682008\114765\524432\718999\1089677\121380\546552\388395"

-- >>> runRandom newSeed (arbitrary :: Rnd Random [Int])
-- [7319755751108929560,7047238280003502157,6285911605709176976,-288376996935305065,6381265667147014285,-2312468355179750876,3304438790729783032,5126308203624262955]

-- >>> runRandom newSeed (arbitrary :: Rnd Random [SmallInt])
-- [7319755751108929560,7047238280003502157,6285911605709176976,-288376996935305065,6381265667147014285,-2312468355179750876,3304438790729783032,5126308203624262955]

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
    printableChrs <- (arbitrary :: Rnd Random [PrintableChar])
    return $ PrintableString (getPrintableChar <$> printableChrs)

instance Show PrintableString where
  show (PrintableString s) = show s

newtype AlphaNumChar = AlphaNumChar {getAlphaNumChar :: Char} deriving (Eq)

instance Arbitrary AlphaNumChar where
  arbitrary = AlphaNumChar <$> randomSample 'a' (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

newtype AlphaNumString = AlphaNumString {getAlphaNumString :: String} deriving (Eq)

instance Arbitrary AlphaNumString where
  arbitrary = do
    printableChrs <- (arbitrary :: Rnd Random [AlphaNumChar])
    return $ AlphaNumString (getAlphaNumChar <$> printableChrs)

instance Show AlphaNumString where
  show (AlphaNumString s) = show s

newtype SortedList a = SortedList {getSortedList :: [a]} deriving (Eq, Ord)

instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
  arbitrary = do
    SortedList . sort <$> (arbitrary :: Gen [a])

instance (Show a) => Show (SortedList a) where
  show (SortedList l) = show l
