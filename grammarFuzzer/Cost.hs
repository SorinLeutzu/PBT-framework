module GrammarFuzzer.Cost where

import Data.Containers.ListUtils as L
import Data.Graph
import Data.List

import Data.Map qualified as M

import GrammarFuzzer.GrammarDefinitions


-- getSymbolCostFromExpansion :: Grammar -> M.Map Symbol Cost -> M.Map (Symbol,Int) Cost
-- getSymbolCostFromExpansion

-- updateCosts :: Grammar -> (M.Map Symbol Cost, M.Map (Symbol,Int) Cost)
-- updateCosts grammar symbolCosts expansionsCosts = do
--   let map1 :: M.Map Symbol Cost = M.empty
--   let map2 :: M.Map (Symbol,Int) Cost = M.empty



-- determineCosts
getCostOfInstantlyTerminating :: Grammar -> Symbol -> Cost
getCostOfInstantlyTerminating grammar symbol = expansionCostMin grammar $ minimum $ filter (all (\(sym, Range lo _) -> isTerminal sym || lo == 0)) (getExpansionsForNonTerminal grammar symbol)

computeAllMinCosts :: Grammar -> M.Map Symbol Cost
computeAllMinCosts grammar = fixpoint initialCosts
  where
    allNonTerminals = getDeclaredNonTerminals grammar
    initialCosts = M.fromList [(nt, Infinite) | nt <- allNonTerminals]

    fixpoint costs =
      let newCosts = M.fromList [(nt, computeMinCost costs nt) | nt <- allNonTerminals]
      in if newCosts == costs then costs else fixpoint newCosts

    computeMinCost costs symbol =
      let expansions = getExpansionsForNonTerminal grammar symbol
      in if null expansions then Infinite
         else minimum $ map (computeExpMinCost costs) expansions

    computeExpMinCost costs expansion =
      foldl addCost (Finite 0) $ map entryCost expansion
      where
        entryCost (sym, Range lo _)
          | isTerminal sym = Finite lo
          | otherwise = multiplyCost lo (M.findWithDefault Infinite sym costs)

symbolCostMin :: Grammar -> Symbol -> Cost
symbolCostMin _ (T _) = Finite 1
symbolCostMin grammar symbol = M.findWithDefault Infinite symbol (computeAllMinCosts grammar)

expansionCostMin :: Grammar -> Expansion -> Cost
expansionCostMin grammar expansion = foldl addCost (Finite 0) $ map entryCostMin expansion
  where
    entryCostMin (sym, Range lo _) = multiplyCost lo (symbolCostMin grammar sym)

-- max cost of stuff:


-- symbolCostMax :: Grammar -> Symbol -> Cost
-- symbolCostMax grammar symbol | symbol `elem` (getTrulyNonTerminatingTerminals) = Infinite
--                              | symbol  `elem` (get)


-- indexSymbols :: [Symbol] -> Int -> M.Map Int Symbol -> M.Map Int Symbol
-- indexSymbols (head : tail) currentIndex map' = indexSymbols tail (currentIndex+1) (M.insert currentIndex head map')
-- indexSymbols [] _ map' = map'

-- indexSymbols' :: [Symbol] -> M.Map Int Symbol
-- indexSymbols' symbolList = indexSymbols symbolList 0 M.empty
-- intListToSymbol =

-- graph :: (Graph, Vertex -> (Int, String, [Int]))
-- graph = graphFromEdges' [ (1, "A", [2,3]), (2, "B", [3]), (3, "C", []) ]
nonTerminalsToInts :: Grammar  -> M.Map Symbol Int
nonTerminalsToInts grammar = let  naturals = [0..]
                                  nonTerminals = getDeclaredNonTerminals grammar
                                                  in M.fromList $ zip nonTerminals naturals
nonTerminalToInt :: Grammar -> Symbol -> Int
nonTerminalToInt grammar symbol =case M.lookup symbol $ nonTerminalsToInts grammar of
  Just x -> x
  Nothing -> -1

intToNonTerminal :: Grammar -> Int -> Symbol
intToNonTerminal grammar int = (getDeclaredNonTerminals grammar) !! int

eraseTerminalsFromExpansion :: Expansion -> [Symbol]
eraseTerminalsFromExpansion expansion = [sym | (sym, Range _ hi) <- expansion, not (isTerminal sym), hi > 0]

prodRuleToEdges :: Grammar ->(Symbol,[Expansion]) -> [Edge]
prodRuleToEdges grammar (symbolToExpand, expansionList) = let cleanedExpansions = map eraseTerminalsFromExpansion expansionList
                                                              reachable :: [Symbol] = nub $ concat cleanedExpansions
                                                              int1 = (nonTerminalToInt grammar symbolToExpand)
                                                              intList = map (nonTerminalToInt grammar) reachable
                                                                    in map (\int2->  (int1, int2)::Edge) intList

transformGrammarToGraph :: Grammar -> [Edge]
transformGrammarToGraph grammar@(Grammar productionRules start) = concatMap (prodRuleToEdges grammar) productionRules

-- finds all cycles in a directed graph represented by an edge list.
findCycles :: [(Int, Int)] -> [[Int]]
findCycles edges =
    -- builds graph nodes: (vertex, vertex, [neighbors])
    let nodes = map (\(a, b) -> (a, a, [b])) edges
    -- get strongly connected components
        sccs = stronglyConnComp nodes
    -- filter out the components that are not single nodes (cycles)
        cycles = [ flattenSCC scc | scc <- sccs, isCycle scc ]
        isCycle (CyclicSCC _) = True
        isCycle _             = False
    in cycles

-- getSymbolsWithInfiniteMaximumCost :: Grammar -> [Symbol]
-- getSymbolsWithInfiniteMaximumCost grammar = map ((getDeclaredNonTerminals grammar) !!) $ L.nubOrd $ concat $ findCycles $ transformGrammarToGraph grammar

getSymbolsWithInfiniteMaximumCost :: Grammar -> [Symbol]
getSymbolsWithInfiniteMaximumCost grammar =
  let nonTerms = getDeclaredNonTerminals grammar          -- list of symbols
      n = length nonTerms
      edges = transformGrammarToGraph grammar             -- [(Int,Int)]
      graph = buildG (0, n-1) edges                       -- forward graph
      revGraph = transposeG graph                          -- reverse graph
      cycles = findCycles edges                            -- list of cycles
      cycleNodes = concat cycles                           -- all cycle vertices
      -- all vertices that can reach any cycle node (including cycle nodes themselves)
      reachableFromCycles = nub $ concatMap (reachable revGraph) cycleNodes
  in map (nonTerms !!) reachableFromCycles

symbolCostMax :: Grammar -> Symbol -> Cost
symbolCostMax _ (T _) = Finite 1
symbolCostMax grammar symbol | symbol `elem` (  getSymbolsWithInfiniteMaximumCost grammar) = Infinite
symbolCostMax grammar symbol = maximum $  map (expansionCostMax grammar) (getExpansionsForNonTerminal grammar symbol)

expansionCostMax :: Grammar -> Expansion -> Cost
expansionCostMax grammar expansion = foldl addCost (Finite 0) $ map entryCostMax expansion
  where
    entryCostMax (sym, Range _ hi) = multiplyCost hi (symbolCostMax grammar sym)