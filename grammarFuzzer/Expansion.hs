module GrammarFuzzer.Expansion where

import Data.List

import Random.Core hiding (GrammarFuzzedString)
import Control.Monad (foldM)

import GrammarFuzzer.GrammarDefinitions
import GrammarFuzzer.Cost


realizeExpansion :: Expansion -> Rnd RealizedExpansion
realizeExpansion = fmap concat . mapM realizeEntry
  where
    realizeEntry (sym, Range lo hi) = do
      count <- nextIntRange lo hi
      return $ replicate count sym

realizeExpansionMin :: Expansion -> RealizedExpansion
realizeExpansionMin = concatMap (\(sym, Range lo _) -> replicate lo sym)

realizeExpansionMax :: Expansion -> RealizedExpansion
realizeExpansionMax = concatMap (\(sym, Range _ hi) -> replicate hi sym)

-- a node is expandable if it is a leaf and a non terminal
getExpandableNodes :: ParseTree -> [ParseTree]
getExpandableNodes (Tree parent children) = concat $  map getExpandableNodes children
getExpandableNodes (Leaf (T t) _) = []
getExpandableNodes leaf@(Leaf (N n)_) = [leaf]

constructEmptyParseTree :: Symbol -> ParseTree
constructEmptyParseTree startSymbol = Leaf startSymbol []

-- takes a leaf and a realized expansion and returns the expanded leaf by that expansion, which is a tree
expandLeaf ::  ParseTree -> RealizedExpansion -> ParseTree
expandLeaf chosenLeaf@(Leaf leafValue pathToLeaf)  expansion = let  childrenIndexes = [0..(length expansion) - 1]
                                                                    newChildren ::[ParseTree] = map (\index -> Leaf (expansion !! index) (pathToLeaf ++ [index]) ) childrenIndexes
                                                                        in Tree leafValue  newChildren

-- takes a tree, a grammar and a leaf to expand and returns the new tree
replaceLeafWithExpansion :: ParseTree -> Grammar -> ParseTree -> RealizedExpansion -> ParseTree
replaceLeafWithExpansion givenTree@(Tree root children) grammar givenLeaf@(Leaf leafValue pathToLeaf) expansion = let childrenAfterReplace = map (\child -> replaceLeafWithExpansion child grammar givenLeaf expansion) children
                                                                                                   in Tree root childrenAfterReplace
replaceLeafWithExpansion leaf@(Leaf leafValue' pathToLeaf') grammar givenLeaf@(Leaf leafValue pathToLeaf) expansion | leafValue == leafValue' && pathToLeaf == pathToLeaf' = expandLeaf leaf expansion
replaceLeafWithExpansion leaf@(Leaf _ _) _ _ _ = leaf

--getExpansionsForNonTerminal grammar leafValue))
getLeafWithMinimumCost :: ParseTree -> Grammar -> ParseTree
getLeafWithMinimumCost parseTree grammar =
  let expandables :: [ParseTree] = getExpandableNodes parseTree
      mini :: Cost = minimum $ map (symbolCostMin grammar) $ map leafValue expandables
      minLeaf = head $ filter (\expandable@(Leaf val _) -> symbolCostMin grammar val == mini) expandables

  in minLeaf

getLeafWithMaximumCost :: ParseTree -> Grammar -> ParseTree
getLeafWithMaximumCost parseTree grammar =
  let expandables :: [ParseTree] = getExpandableNodes parseTree
      maxi :: Cost = maximum $ map (symbolCostMax grammar) $ map leafValue expandables
      maxLeaf = head $ filter (\expandable@(Leaf val _) -> symbolCostMax grammar val == maxi) expandables

  in maxLeaf

getExpansionWithMinimumCost ::  Symbol -> Grammar -> Expansion
getExpansionWithMinimumCost symbol grammar = let mini = minimum $ map (expansionCostMin grammar) $ getExpansionsForNonTerminal grammar symbol
                                               in head $ filter (\expansion-> (expansionCostMin grammar expansion) == mini )  $ getExpansionsForNonTerminal grammar symbol

getExpansionWithMaximumCost :: Symbol -> Grammar -> Expansion
getExpansionWithMaximumCost  symbol grammar =
    let expansions = getExpansionsForNonTerminal grammar symbol
        maxi = maximum $ map (expansionCostMax grammar) expansions
        maxExpansions = filter (\expansion -> (expansionCostMax grammar expansion) == maxi) expansions
        countNonTerminals exp = length $ filter (\(sym, Range _ hi) -> not (isTerminal sym) && hi > 0) exp
    in head $ sortBy (\a b -> compare (countNonTerminals b) (countNonTerminals a)) maxExpansions



replaceMinimumCostLeafWithExpansion :: ParseTree -> Grammar -> ParseTree
replaceMinimumCostLeafWithExpansion initialParseTree grammar =
  let chosenLeaf = getLeafWithMinimumCost initialParseTree grammar
      chosenExpansion = getExpansionWithMinimumCost (leafValue chosenLeaf) grammar
      realized = realizeExpansionMin chosenExpansion
      in replaceLeafWithExpansion initialParseTree grammar chosenLeaf realized

replaceMinimumCostLeafWithExpansionRandom :: ParseTree -> Grammar -> Rnd ParseTree
replaceMinimumCostLeafWithExpansionRandom initialParseTree grammar = do
  let expandables = getExpandableNodes initialParseTree
      mini = minimum $ map (symbolCostMin grammar) $ map leafValue expandables
      minLeaves = filter (\(Leaf val _) -> symbolCostMin grammar val == mini) expandables
  chosenLeaf <- randomElement minLeaves
  let expansions = getExpansionsForNonTerminal grammar (leafValue chosenLeaf)
      miniExp = minimum $ map (expansionCostMin grammar) expansions
      minExpansions = filter (\expansion -> expansionCostMin grammar expansion == miniExp) expansions
  chosenExpansion <- randomElement minExpansions
  let realized = realizeExpansionMin chosenExpansion
  return $ replaceLeafWithExpansion initialParseTree grammar chosenLeaf realized

replaceMaximumCostLeafWithExpansion :: ParseTree -> Grammar -> ParseTree
replaceMaximumCostLeafWithExpansion initialParseTree grammar =
  let chosenLeaf = getLeafWithMaximumCost initialParseTree grammar
      chosenExpansion = getExpansionWithMaximumCost (leafValue chosenLeaf) grammar
      realized = realizeExpansionMax chosenExpansion
      in replaceLeafWithExpansion initialParseTree grammar chosenLeaf realized


-- replace random leaf in a tree with a random expansion and return the updated tree
replaceLeafWithExpansionRandom :: ParseTree -> Grammar -> Rnd ParseTree
replaceLeafWithExpansionRandom initialParseTree grammar = do
  let leaves = getExpandableNodes initialParseTree
  if null leaves then
    return initialParseTree
    else  do
          chosenLeaf <- randomElement leaves
          let expansions = getExpansionsForNonTerminal grammar (leafValue chosenLeaf)
          chosenExpansion <- randomElement expansions
          realized <- realizeExpansion chosenExpansion
          return $ replaceLeafWithExpansion initialParseTree grammar chosenLeaf realized

-- like replaceLeafWithExpansionRandom but prefers non-terminating expansions
-- (those containing at least one non-terminal with maxRep > 0) so the tree keeps growing
replaceLeafWithNonTerminatingExpansionRandom :: ParseTree -> Grammar -> Rnd ParseTree
replaceLeafWithNonTerminatingExpansionRandom initialParseTree grammar = do
  let leaves = getExpandableNodes initialParseTree
  if null leaves then
    return initialParseTree
    else do
          chosenLeaf <- randomElement leaves
          let allExpansions = getExpansionsForNonTerminal grammar (leafValue chosenLeaf)
              nonTerminating = filter (any (\(sym, Range _ hi) -> not (isTerminal sym) && hi > 0)) allExpansions
              expansions = if null nonTerminating then allExpansions else nonTerminating
          chosenExpansion <- randomElement expansions
          realized <- realizeExpansion chosenExpansion
          return $ replaceLeafWithExpansion initialParseTree grammar chosenLeaf realized

-- apply a given number of random expansions
applyRandomExpansions :: Int -> ParseTree -> Grammar -> Rnd ParseTree
applyRandomExpansions n tree grammar =
    foldM (\currentTree _ -> replaceLeafWithExpansionRandom currentTree grammar) tree [1..n]


applyMinimumCostExpansions :: Int -> ParseTree -> Grammar -> ParseTree
applyMinimumCostExpansions n tree grammar = foldl (\currentTree _-> replaceMinimumCostLeafWithExpansion currentTree grammar) tree [1..n]

applyMaximumCostExpansions :: Int -> ParseTree -> Grammar -> ParseTree
applyMaximumCostExpansions n tree grammar = foldl (\currentTree _-> replaceMaximumCostLeafWithExpansion currentTree grammar) tree [1..n]

-- chooses which kind of expansion to perform (minimum, random, maximum) based on the number of non terminals leaves (expandable nodes) in the parse tree
-- if the number is below minNonTerminals it chooses maximum expansion
-- if it s between minNonTerminals and maxNonTerminals it chooses randomExpansion
-- if it s greater than maxNonTerminals it chooses mininum expansion until no more leaves are expandable
data ExpansionStrategy = Minimum | Random | Maximum
  deriving (Show, Eq)