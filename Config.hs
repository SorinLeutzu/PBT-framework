module Config where

data ExecutionMode = SequentialExec | ParallelExec deriving (Show, Eq)

data ShrinkingImpl = Sequential | DeterministicParallel | NonDeterministicParallel deriving (Show, Eq)

data TupleShrinkStrategy = PerDimension | AllDimensions deriving (Show, Eq)

shrinkingImplementation :: ShrinkingImpl
shrinkingImplementation = DeterministicParallel

tupleShrinkStrategy :: TupleShrinkStrategy
tupleShrinkStrategy = PerDimension

chunkSize :: Int
chunkSize = 20

withColors :: Bool
withColors = True

withEmojis :: Bool
withEmojis = True

executionMode :: ExecutionMode
executionMode = ParallelExec

numThreads :: Int
numThreads = 4