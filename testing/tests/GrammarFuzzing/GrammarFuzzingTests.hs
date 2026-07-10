module Main where

import Data.List ((\\))

import Random.Core hiding (GrammarFuzzedString)
import GrammarFuzzer.GrammarDefinitions
import GrammarFuzzer.Cost (symbolCostMax)
import GrammarFuzzer.Expansion
  ( getExpandableNodes
  , replaceLeafWithNonTerminatingExpansionRandom
  , replaceMinimumCostLeafWithExpansionRandom
  )
import GrammarFuzzer.Construction (phaseMaximum)
import GrammarFuzzer.Validation
  ( checkIfInstantlyTerminates
  , getInstantlyTerminatingNonTerminals
  , getEventuallyTerminatingNonTerminals
  )
import GrammarFuzzer.Examples (grammarArithmetic, grammarParens)

report :: String -> [(String, Bool)] -> IO ()
report title checks = do
  putStrLn ("== " ++ title ++ " ==")
  mapM_ (\(name, ok) -> putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)) checks
  let total  = length checks
      passed = length (filter snd checks)
  putStrLn (replicate 50 '-')
  putStrLn ("Passed " ++ show passed ++ " / " ++ show total)

gMin0 :: Grammar
gMin0 = Grammar [ (N "S", [ [s (T "a"), r 0 3 (N "S")] ]) ] (N "S")

gMin1 :: Grammar
gMin1 = Grammar
  [ (N "S", [ [s (T "a"), r 1 3 (N "X")] ])
  , (N "X", [ [s (T "b")] ])
  ] (N "S")

gOpt :: Grammar
gOpt = Grammar
  [ (N "S", [ [s (T "a"), opt (N "X")] ])
  , (N "X", [ [s (T "b")] ])
  ] (N "S")

terminationChecks :: [(String, Bool)]
terminationChecks =
  [ ("minRep=0 recursive: instantly terminates",
       checkIfInstantlyTerminates gMin0 (N "S"))
  , ("minRep=1 recursive: not instantly terminating",
       not (checkIfInstantlyTerminates gMin1 (N "S")))
  , ("minRep=1 recursive: eventually terminating",
       N "S" `elem` getEventuallyTerminatingNonTerminals gMin1
                       (getInstantlyTerminatingNonTerminals gMin1))
  , ("optional non-terminal: instantly terminates",
       checkIfInstantlyTerminates gOpt (N "S"))
  ]

validStart :: Grammar -> Bool
validStart = checkStartingPoint

hasUndeclared :: Grammar -> Bool
hasUndeclared g = not (null (getUsedNonTerminals g \\ getDeclaredNonTerminals g))

hasUnreachable :: Grammar -> Bool
hasUnreachable g = not (null (getDeclaredNonTerminals g \\ getUsedNonTerminals g))

wellFormed :: Grammar -> Bool
wellFormed g = validStart g && not (hasUndeclared g) && not (hasUnreachable g)

gValid1, gValid2, gBadStart1, gBadStart2, gUndeclared1, gUndeclared2,
  gUnreachable1, gUnreachable2, gMixed, gAllGood :: Grammar
gValid1 = Grammar [ (N "A", [ [s (T "a"), s (N "B")] ]), (N "B", [ [s (T "b")] ]) ] (N "A")
gValid2 = Grammar [ (N "S", [ [s (T "a")] ]) ] (N "S")
gBadStart1 = Grammar [ (N "A", [ [s (T "a")] ]) ] (N "Z")
gBadStart2 = Grammar [ (N "A", [ [s (T "a"), s (N "B")] ]), (N "B", [ [s (T "b")] ]) ] (N "C")
gUndeclared1 = Grammar [ (N "A", [ [s (T "a"), s (N "B")] ]) ] (N "A")
gUndeclared2 = Grammar [ (N "S", [ [s (T "a"), s (N "T")] ]), (N "T", [ [s (T "c"), s (N "U")] ]) ] (N "S")
gUnreachable1 = Grammar [ (N "A", [ [s (T "a")] ]), (N "B", [ [s (T "b")] ]) ] (N "A")
gUnreachable2 = Grammar [ (N "S", [ [s (T "a")] ]), (N "X", [ [s (T "b")] ]), (N "Y", [ [s (T "c")] ]) ] (N "S")
gMixed = Grammar [ (N "A", [ [s (T "a"), s (N "B")] ]), (N "B", [ [s (T "b")] ]), (N "C", [ [s (T "c")] ]) ] (N "A")
gAllGood = Grammar [ (N "A", [ [s (T "a"), s (N "A")], [s (T "b")] ]) ] (N "A")

validityChecks :: [(String, Bool)]
validityChecks =
  [ ("gValid1 is well-formed",                       wellFormed gValid1)
  , ("gValid2 is well-formed",                       wellFormed gValid2)
  , ("gBadStart1 invalid start detected",       not (validStart gBadStart1))
  , ("gBadStart2 invalid start detected",       not (validStart gBadStart2))
  , ("gUndeclared1 undeclared non-terminal",         hasUndeclared gUndeclared1)
  , ("gUndeclared2 undeclared non-terminal",         hasUndeclared gUndeclared2)
  , ("gUnreachable1 unreachable non-terminal",       hasUnreachable gUnreachable1)
  , ("gUnreachable2 unreachable non-terminal",       hasUnreachable gUnreachable2)
  , ("gMixed unreachable C, otherwise valid",        hasUnreachable gMixed && validStart gMixed && not (hasUndeclared gMixed))
  , ("gAllGood is well-formed",                      wellFormed gAllGood)
  ]

isInfiniteMax :: Grammar -> Symbol -> Bool
isInfiniteMax g sym = symbolCostMax g sym == Infinite

gDag :: Grammar
gDag = Grammar
  [ (N "A", [ [s (N "B")] ])
  , (N "B", [ [s (N "C")] ])
  , (N "C", [ [s (N "D")] ])
  , (N "D", [ [s (T "x")] ])
  ] (N "A")

gSelfLoop :: Grammar
gSelfLoop = Grammar
  [ (N "A", [ [s (T "a"), s (N "A")], [s (T "a")] ])
  , (N "B", [ [s (N "C")] ])
  , (N "C", [ [s (N "B")] ])
  , (N "D", [ [s (T "d")] ])
  ] (N "A")

gReachesCycle :: Grammar
gReachesCycle = Grammar
  [ (N "A", [ [s (N "B")] ])
  , (N "B", [ [s (N "C")] ])
  , (N "C", [ [s (N "B")] ])
  , (N "D", [ [s (T "d")] ])
  ] (N "A")

costChecks :: [(String, Bool)]
costChecks =
  [ ("gDag " ++ show nt ++ " has finite max cost", not (isInfiniteMax gDag nt))
  | nt <- [N "A", N "B", N "C", N "D"] ]
  ++
  [ ("gSelfLoop A infinite",  isInfiniteMax gSelfLoop (N "A"))
  , ("gSelfLoop B infinite",  isInfiniteMax gSelfLoop (N "B"))
  , ("gSelfLoop C infinite",  isInfiniteMax gSelfLoop (N "C"))
  , ("gSelfLoop D finite", not (isInfiniteMax gSelfLoop (N "D")))
  ]
  ++
  [ ("gReachesCycle A infinite",  isInfiniteMax gReachesCycle (N "A"))
  , ("gReachesCycle B infinite",  isInfiniteMax gReachesCycle (N "B"))
  , ("gReachesCycle C infinite",  isInfiniteMax gReachesCycle (N "C"))
  , ("gReachesCycle D finite", not (isInfiniteMax gReachesCycle (N "D")))
  ]

minNT, nRandom :: Int
minNT   = 4
nRandom = 5

phase2 :: PRNG -> Grammar -> ParseTree -> Int -> (ParseTree, Int)
phase2 _ _ tree 0 = (tree, 0)
phase2 prng grammar tree k
  | null (getExpandableNodes tree) = (tree, 0)
  | otherwise =
      let (tree', prng') = runRandom (replaceLeafWithNonTerminatingExpansionRandom tree grammar) prng
          (final, done)  = phase2 prng' grammar tree' (k - 1)
      in (final, done + 1)

phase3 :: PRNG -> Grammar -> ParseTree -> ParseTree
phase3 prng grammar tree
  | null (getExpandableNodes tree) = tree
  | otherwise =
      let (tree', prng') = runRandom (replaceMinimumCostLeafWithExpansionRandom tree grammar) prng
      in phase3 prng' grammar tree'

phaseChecks :: String -> Grammar -> PRNG -> [(String, Bool)]
phaseChecks name grammar prng =
  let initial    = Leaf (startingPoint grammar) []
      afterMax   = phaseMaximum grammar minNT initial
      count1     = length (getExpandableNodes afterMax)
      (afterRnd, count2) = phase2 prng grammar afterMax nRandom
      afterMin   = phase3 prng grammar afterRnd
  in [ (name ++ " phase 1: >= " ++ show minNT ++ " expandable leaves", count1 >= minNT)
     , (name ++ " phase 2: exactly " ++ show nRandom ++ " random expansions",
          count2 == nRandom || null (getExpandableNodes afterRnd))
     , (name ++ " phase 3: all leaves are terminals", null (getExpandableNodes afterMin))
     ]

gBinaryTree :: Grammar
gBinaryTree = Grammar [ (N "T", [ [s (N "T"), s (N "T")], [s (T "leaf")] ]) ] (N "T")

seeds :: [PRNG]
seeds = [PRNG_Xor seedXor, PRNG_Mersenne seedMersenne, PRNG_Pcg seedPcg]

constructionChecks :: [(String, Bool)]
constructionChecks = concat
  [ phaseChecks (gname ++ " seed" ++ show i) g prng
  | (gname, g) <- [("arith", grammarArithmetic), ("parens", grammarParens), ("tree", gBinaryTree)]
  , (i, prng)  <- zip [1 :: Int ..] seeds
  ]

main :: IO ()
main = report "6.1.4 Grammar fuzzing"
  (  terminationChecks
  ++ validityChecks
  ++ costChecks
  ++ constructionChecks
  )