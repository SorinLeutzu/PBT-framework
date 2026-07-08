module Main where

import Matchers.Core
import Matchers.Combinators

report :: String -> [(String, Bool)] -> IO ()
report title checks = do
  putStrLn ("== " ++ title ++ " ==")
  mapM_ (\(name, ok) -> putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)) checks
  let total  = length checks
      passed = length (filter snd checks)
  putStrLn (replicate 50 '-')
  putStrLn ("Passed " ++ show passed ++ " / " ++ show total)

goodBad :: String -> Bool -> Bool -> (String, Bool)
goodBad label good bad = (label, good && not bad)

idempotenceChecks :: [(String, Bool)]
idempotenceChecks =
  [ goodBad ("idempotence (id vs (+1)) @ " ++ show x)
      (matches (Idempotence (id :: Int -> Int) 5) x)
      (matches (Idempotence ((+ 1) :: Int -> Int) 5) x)
  | x <- [1 .. 5 :: Int] ]

invertibilityChecks :: [(String, Bool)]
invertibilityChecks =
  [ goodBad ("invertibility ((+1)/subtract 1 vs id/(+1)) @ " ++ show x)
      (matches (Invertibility ((+ 1) :: Int -> Int) (subtract 1 :: Int -> Int)) x)
      (matches (Invertibility (id :: Int -> Int) ((+ 1) :: Int -> Int)) x)
  | x <- [1 .. 5 :: Int] ]

associativityChecks :: [(String, Bool)]
associativityChecks =
  [ goodBad ("associativity ((+) vs (-)) @ " ++ show t)
      (matches (Associativity ((+) :: Int -> Int -> Int)) t)
      (matches (Associativity ((-) :: Int -> Int -> Int)) t)
  | t <- [(1,2,3),(2,3,4),(4,1,5),(7,2,3),(5,5,1)] :: [(Int,Int,Int)] ]

commutativityChecks :: [(String, Bool)]
commutativityChecks =
  [ goodBad ("commutativity ((+) vs (-)) @ " ++ show p)
      (matches (Commutativity ((+) :: Int -> Int -> Int)) p)
      (matches (Commutativity ((-) :: Int -> Int -> Int)) p)
  | p <- [(1,2),(3,4),(5,1),(7,2),(9,4)] :: [(Int,Int)] ]

distributivityChecks :: [(String, Bool)]
distributivityChecks =
  [ ("distributivity (*) over (+) @ " ++ show t,
       matches (Distributivity ((+) :: Int -> Int -> Int) ((*) :: Int -> Int -> Int)) t)
  | t <- triples ]
  ++
  [ ("distributivity (-) over (+) fails @ " ++ show t,
       not (matches (Distributivity ((+) :: Int -> Int -> Int) ((-) :: Int -> Int -> Int)) t))
  | t <- triples ]
  where triples = [(2,3,4),(1,5,6),(3,2,7),(4,4,1),(5,1,2)] :: [(Int,Int,Int)]

m :: GtMatcher Int
m = GtMatcher 0

andIdempotenceChecks :: [(String, Bool)]
andIdempotenceChecks =
  [ ("And(m,m) == m @ " ++ show x,
       matches (And (GtMatcher (0 :: Int)) (GtMatcher (0 :: Int)) :: And Int) x
         == matches m x)
  | x <- vals ]

excludedMiddleChecks :: [(String, Bool)]
excludedMiddleChecks =
  [ ("Or(m, Not m) always holds @ " ++ show x,
       matches (Or (GtMatcher (0 :: Int)) (Not (GtMatcher (0 :: Int)) :: Not Int) :: Or Int) x)
  | x <- vals ]

contradictionChecks :: [(String, Bool)]
contradictionChecks =
  [ ("And(m, Not m) never holds @ " ++ show x,
       not (matches (And (GtMatcher (0 :: Int)) (Not (GtMatcher (0 :: Int)) :: Not Int) :: And Int) x))
  | x <- vals ]

doubleNegationChecks :: [(String, Bool)]
doubleNegationChecks =
  [ ("Not(Not m) == m @ " ++ show x,
       matches (Not (Not (GtMatcher (0 :: Int)) :: Not Int) :: Not Int) x == matches m x)
  | x <- vals ]

deMorganChecks :: [(String, Bool)]
deMorganChecks =
  [ ("De Morgan @ " ++ show x,
       matches (Not (And (GtMatcher (0 :: Int)) (LtMatcher (2 :: Int)) :: And Int) :: Not Int) x
         == matches (Or (Not (GtMatcher (0 :: Int)) :: Not Int)
                        (Not (LtMatcher (2 :: Int)) :: Not Int) :: Or Int) x)
  | x <- [-1, 0, 1, 2, 3 :: Int] ]

vals :: [Int]
vals = [-2, -1, 0, 1, 2]

simpleMatcherChecks :: [(String, Bool)]
simpleMatcherChecks =
  [ ("EqMatcher 5 == 5",            matches (EqMatcher (5 :: Int)) (5 :: Int))
  , ("EqMatcher 5 /= 4",       not (matches (EqMatcher (5 :: Int)) (4 :: Int)))
  , ("GtMatcher 3 < 5",             matches (GtMatcher (3 :: Int)) (5 :: Int))
  , ("GtMatcher 5 not < 3",    not (matches (GtMatcher (5 :: Int)) (3 :: Int)))
  , ("LtMatcher 10 > 5",            matches (LtMatcher (10 :: Int)) (5 :: Int))
  , ("LtMatcher 3 not > 5",    not (matches (LtMatcher (3 :: Int)) (5 :: Int)))
  , ("Contains \"ell\" in \"hello\"", matches (ContainsMatcher "ell") "hello")
  , ("StartsWith \"he\" in \"hello\"", matches (StartsWithMatcher "he") "hello")
  , ("EndsWith \"lo\" in \"hello\"",   matches (EndsWithMatcher "lo") "hello")
  , ("ContainsElem 3 in [1,2,3]",   matches (ContainsElem (3 :: Int)) ([1, 2, 3] :: [Int]))
  ]

comp1 :: Composite Int
comp1 = AllOf [ One (AnyMatcher (GtMatcher (0 :: Int)))
              , NotOf (One (AnyMatcher (EqMatcher (7 :: Int)))) ]

comp2 :: Composite Int
comp2 = AnyOf [ One (AnyMatcher (EqMatcher (1 :: Int)))
              , One (AnyMatcher (EqMatcher (2 :: Int))) ]

nested :: Composite Int
nested = AnyOf [ comp1, AllOf [ One (AnyMatcher (EqMatcher (42 :: Int))) ] ]

multiAll :: MultiComposite
multiAll = AllOfM [ OneM (AnyMatch (EqMatcher (5 :: Int)) (5 :: Int))
                  , OneM (AnyMatch (ContainsMatcher "ell") ("hello" :: String)) ]

multiBad :: MultiComposite
multiBad = AllOfM [ OneM (AnyMatch (EqMatcher (5 :: Int)) (5 :: Int))
                  , OneM (AnyMatch (EqMatcher (9 :: Int)) (0 :: Int)) ]

multiAny :: MultiComposite
multiAny = AnyOfM [ OneM (AnyMatch (EqMatcher (9 :: Int)) (0 :: Int))
                  , NotOfM (OneM (AnyMatch (EndsWithMatcher "xx") ("hello" :: String))) ]

compositeMatcherChecks :: [(String, Bool)]
compositeMatcherChecks =
  [ ("comp1 (x>0 and x/=7) holds for 3",       matches comp1 (3 :: Int))
  , ("comp1 fails for 7",                  not (matches comp1 (7 :: Int)))
  , ("comp1 fails for -1",                 not (matches comp1 (-1 :: Int)))
  , ("comp2 (x==1 or x==2) holds for 2",       matches comp2 (2 :: Int))
  , ("comp2 fails for 3",                  not (matches comp2 (3 :: Int)))
  , ("nested holds for 42",                    matches nested (42 :: Int))
  , ("nested fails for -1",                not (matches nested (-1 :: Int)))
  , ("multiAll (all leaves succeed)",          matches multiAll ())
  , ("multiBad (one leaf fails)",          not (matches multiBad ()))
  , ("multiAny (one branch succeeds)",         matches multiAny ())
  ]

main :: IO ()
main = report "6.1.2 Assertions"
  (  idempotenceChecks
  ++ invertibilityChecks
  ++ associativityChecks
  ++ commutativityChecks
  ++ distributivityChecks
  ++ andIdempotenceChecks
  ++ excludedMiddleChecks
  ++ contradictionChecks
  ++ doubleNegationChecks
  ++ deMorganChecks
  ++ simpleMatcherChecks
  ++ compositeMatcherChecks
  )