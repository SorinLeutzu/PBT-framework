module GrammarFuzzer.GrammarDefinitions where

import Data.List

data Symbol = N String| T String deriving (Eq,Ord)

data Range = Range { minRep :: Int, maxRep :: Int } deriving (Show, Eq, Ord)

type SymbolEntry = (Symbol, Range)
type Expansion  = [SymbolEntry]
type RealizedExpansion = [Symbol]
type DerivationsForNonTerminal = (Symbol, [Expansion])

--  symbol that appears exactly once
s :: Symbol -> SymbolEntry
s sym = (sym, Range 1 1)

-- symbol whose apparitions are in repetition range [lo..hi]
r :: Int -> Int -> Symbol -> SymbolEntry
r lo hi sym = (sym, Range lo hi)

-- pptional symbol : 0 or 1 apparitions
opt :: Symbol -> SymbolEntry
opt sym = (sym, Range 0 1)


someGrammar :: Grammar
someGrammar = Grammar
  [ (N "A", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "A"), s (T "aaa"), s (N "B")]
           ])
  , (N "B", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "A"), s (T "aaa")]
           ])
  , (N "C", [ [s (T "ab"), s (T "bc")]
           , [s (N "B"), s (T "aaa")]
           ])
  ] (N "A")


data Grammar = Grammar  { derivationRules :: [DerivationsForNonTerminal], startingPoint ::  Symbol } deriving Show

-- instance show Grammar where
--     show (Grammar derivationRules startingPoint) = map (\(nonTerminal, expansionsList)-> show nonTerminal ++ ":" ++  ) derivationRules

instance Show Symbol where
  show (N nt) =  nt
  show (T t) =  t



data Grammar' = Grammar' { grammar :: Grammar, minimumNonTerminals :: Int, randomExpansionCount :: Int }

someGrammar' = Grammar' someGrammar 4 10

newtype GrammarFuzzedString = GrammarFuzzedString { getGrammarFuzzedString :: String } deriving (Eq)

instance Show GrammarFuzzedString where
  show (GrammarFuzzedString s) = show s

data ParseTree = Tree { parent :: Symbol, children :: [ParseTree] } | Leaf { leafValue :: Symbol, pathToLeaf :: [Int]} deriving Show


                                   
data Cost = Finite Int | Infinite deriving (Eq,Show)

addCost :: Cost -> Cost -> Cost
addCost (Finite c) Infinite = Infinite
addCost Infinite (Finite c) = Infinite
addCost (Finite c1) (Finite c2) = Finite (c1+c2)
addCost Infinite Infinite = Infinite

multiplyCost :: Int -> Cost -> Cost
multiplyCost 0 _ = Finite 0
multiplyCost n (Finite c) = Finite (n * c)
multiplyCost _ Infinite = Infinite

instance Ord Cost where
  (<) c1 c2 = c1 == minCost c1 c2 && c1 /= c2
  (<=) c1 c2 = c1 == minCost c1 c2

minCost :: Cost -> Cost -> Cost
minCost (Finite c) Infinite = Finite c
minCost Infinite (Finite c) = Finite c
minCost (Finite c1) (Finite c2) = Finite (min c1 c2)
minCost Infinite Infinite = Infinite

minimumCost :: [Cost]->Cost
minimumCost costs = minimum costs
