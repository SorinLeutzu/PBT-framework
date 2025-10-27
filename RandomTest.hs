{-# LANGUAGE ScopedTypeVariables #-}

module RandomTest where

import Random

takeShrinks :: Int -> (a -> [a]) -> a -> [a]
takeShrinks n shrinker x = take n (shrinker x)

genAndShowShrinks :: forall a. (Arbitrary a) => String -> (a -> String) -> PRNG -> IO ()
genAndShowShrinks typeName printer prng = do
  let val = runRandom prng (arbitrary :: Gen a)
      candidates = takeShrinks 5 shrink val
  putStrLn $ replicate 60 '-'
  putStrLn $ typeName ++ "   (generated with: " ++ show prng ++ ")"
  putStrLn $ "  value: " ++ printer val
  putStrLn $ "  first up to 5 shrink candidates (in order returned by shrink):"
  if null candidates
    then putStrLn "    (none)"
    else mapM_ (\(i, v) -> putStrLn $ "    " ++ show i ++ ") " ++ printer v) (zip [1 :: Int ..] candidates)
  putStrLn ""

genInt :: PRNG -> IO ()
genInt = genAndShowShrinks "Int" (show :: Int -> String)

genBool :: PRNG -> IO ()
genBool = genAndShowShrinks "Bool" (show :: Bool -> String)

genListInt :: PRNG -> IO ()
genListInt = genAndShowShrinks "[Int]" (show :: [Int] -> String)

genChar :: PRNG -> IO ()
genChar = genAndShowShrinks "Char" (show :: Char -> String)

genPairIntBool :: PRNG -> IO ()
genPairIntBool = genAndShowShrinks "(Int, Bool)" (show :: (Int, Bool) -> String)

genTripleInt :: PRNG -> IO ()
genTripleInt = genAndShowShrinks "(Int,Int,Int)" (show :: (Int, Int, Int) -> String)

genSmallInt :: PRNG -> IO ()
genSmallInt = genAndShowShrinks "SmallInt" (show :: SmallInt -> String)

-- PrintableChar: show inner Char
genPrintableChar :: PRNG -> IO ()
genPrintableChar = genAndShowShrinks "PrintableChar" (\(PrintableChar c) -> show (c :: Char))

genPrintableString :: PRNG -> IO ()
genPrintableString = genAndShowShrinks "PrintableString" (show :: PrintableString -> String)

-- AlphaNumChar: show inner Char
genAlphaNumChar :: PRNG -> IO ()
genAlphaNumChar = genAndShowShrinks "AlphaNumChar" (\(AlphaNumChar c) -> show (c :: Char))

genAlphaNumString :: PRNG -> IO ()
genAlphaNumString = genAndShowShrinks "AlphaNumString" (show :: AlphaNumString -> String)

genSortedListInt :: PRNG -> IO ()
genSortedListInt = genAndShowShrinks "SortedList Int" (show :: SortedList Int -> String)

runAllForPrng :: PRNG -> IO ()
runAllForPrng prng = do
  putStrLn $ "\n=== Using PRNG: " ++ show prng ++ " ===\n"
  genInt prng
  genBool prng
  genListInt prng
  genChar prng
  genPairIntBool prng
  genTripleInt prng
  genSmallInt prng
  genPrintableChar prng
  genPrintableString prng
  genAlphaNumChar prng
  genAlphaNumString prng
  genSortedListInt prng

main :: IO ()
main = do
  let prngs = [PRNG_Xor seedXor, PRNG_Mersenne seedMersenne, PRNG_Pcg seedPcg]
  mapM_ runAllForPrng prngs
  putStrLn "Done."
