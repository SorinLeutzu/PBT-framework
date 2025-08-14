module BitOps where

shiftL :: (Integral a) => a -> Int -> a
shiftL nr n = nr * 2 ^ n

shiftR :: (Integral a) => a -> Int -> a
shiftR nr n = nr `div` 2 ^ n
