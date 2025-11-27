module Config where

data ShrinkStrategy = Standard | Aggressive

shrinkStrategy :: ShrinkStrategy
shrinkStrategy = Standard

maximumThreadNumber :: Int
maximumThreadNumber = 5

withColors :: Bool
withColors = True

withEmojis :: Bool
withEmojis = True
