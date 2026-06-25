module Config where

data ShrinkingImpl = Sequential | DeterministicParallel | NonDeterministicParallel deriving (Show, Eq)

shrinkingImplementation :: ShrinkingImpl
shrinkingImplementation = DeterministicParallel

chunkSize :: Int
chunkSize = 20

withColors :: Bool
withColors = True

withEmojis :: Bool
withEmojis = True