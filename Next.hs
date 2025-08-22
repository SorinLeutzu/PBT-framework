module Next where

import BitOps (shiftL, shiftR)
import Config
import Data.Bits (Bits (xor, (.&.), (.|.)))
import Data.Word qualified as W

-- Xor
nextXor :: W.Word64 -> W.Word64
nextXor w =
  let a = w `xor` (w `shiftR` 12)
      b = a `xor` (a `shiftR` 25)
      c = b `xor` (b `shiftR` 27)
   in c * 0x2545F4914F6CDD1D

-- Mersenne Twister

n, m :: Int
n = 312
m = 156

matrixA, upperMask, lowerMask :: W.Word64
matrixA = 0xB5026F5AA96619E9
upperMask = 0xFFFFFFFF80000000
lowerMask = 0x7FFFFFFF

initMT :: W.Word64 -> [W.Word64]
initMT seed = take n $ initState 1 [seed]
  where
    f = 6364136223846793005
    initState i state
      | i >= n = state
      | otherwise =
          let prev = state !! (i - 1)
              next = f * (prev `xor` (prev `shiftR` 62)) + fromIntegral i
           in initState (i + 1) (state ++ [next])

twist :: [W.Word64] -> [W.Word64]
twist state = [twistAt i | i <- [0 .. n - 1]]
  where
    twistAt i =
      let x = (state !! i .&. upperMask) + (state !! ((i + 1) `mod` n) .&. lowerMask)
          xA = x `shiftR` 1
          xA' = if (x .&. 1) /= 0 then xA `xor` matrixA else xA
       in (state !! ((i + m) `mod` n)) `xor` xA'

extractNumber :: [W.Word64] -> (W.Word64, [W.Word64])
extractNumber st =
  let state = if null st then initMT 5489 else st
      y1 = head state
      y2 = y1 `xor` ((y1 `shiftR` 29) .&. 0x5555555555555555)
      y3 = y2 `xor` ((y2 `shiftL` 17) .&. 0x71D67FFFEDA60000)
      y4 = y3 `xor` ((y3 `shiftL` 37) .&. 0xFFF7EEE000000000)
      y5 = y4 `xor` (y4 `shiftR` 43)
   in (y5, tail state)

nextMersenne :: W.Word64 -> W.Word64
nextMersenne !seed =
  let st0 = initMT seed
      (val, st1) = extractNumber st0
   in val

-- PCG
nextpcg64 :: W.Word64 -> W.Word64
nextpcg64 !state =
  let newstate = state * 6364136223846793005 + 1442695040888963407
      xorshifted = fromIntegral (((state `shiftR` 18) `xor` state) `shiftR` 27)
      rot = fromIntegral (state `shiftR` 59)
   in (xorshifted `shiftR` rot) .|. (xorshifted `shiftL` ((-rot) .&. 31))
