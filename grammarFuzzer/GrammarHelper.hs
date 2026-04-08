{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module GrammarFuzzer.GrammarHelper where

import Data.Containers.ListUtils as L
import Data.Graph
import Data.List

import Random hiding (GrammarFuzzedString)
import Control.Monad (foldM)

import Data.Map qualified as M


import GrammarFuzzer.GrammarDefinitions


import System.Directory (createDirectoryIfMissing, canonicalizePath)
import System.IO (writeFile)
import System.FilePath ((</>))



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

-- performs basic checks on a given grammar
checkGrammar :: Grammar -> Bool
checkGrammar _ = True

getDeclaredNonTerminals :: Grammar -> [Symbol]
getDeclaredNonTerminals (Grammar derivationRules _) = map (\(nonTerminal,_)->nonTerminal) derivationRules


getUsedNonTerminals :: Grammar -> [Symbol]
getUsedNonTerminals (Grammar derivationRules startingPoint) = L.nubOrd (startingPoint : otherUsedNonTerminals) where
    allSymbols :: [Symbol] = map fst $ concat $ concat $ map snd derivationRules
    otherUsedNonTerminals :: [Symbol] = filter (not . isTerminal) allSymbols


getTerminals :: Grammar -> [Symbol]
getTerminals (Grammar derivationRules _) = terminals where
    allSymbols :: [Symbol] = map fst $ concat $ concat $ map snd derivationRules
    terminals :: [Symbol] = filter isTerminal allSymbols

checkStartingPoint :: Grammar -> Bool
checkStartingPoint  grammar@(Grammar derivationRules startingPoint) = elem startingPoint (getDeclaredNonTerminals grammar)  

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

getExpansionsForNonTerminal :: Grammar -> Symbol -> [Expansion]
getExpansionsForNonTerminal (Grammar derivationRules _) symbol = concat $ concat $ filter (\x-> x/= []) $ map (\x-> if fst x == symbol then [snd x] else []) derivationRules

constructParseTree :: Grammar' -> ParseTree
constructParseTree (Grammar' grammar minNT nRandom) =
    let startSym = startingPoint grammar
        initialTree = Leaf startSym []
        initialPRNG = PRNG_Xor seedXor
        -- Phase 1: maximum expansions until we have at least minNT expandable nodes
        afterMax = phaseMaximum grammar minNT initialTree
        -- Phase 2: nRandom random expansions
        (afterRandom, prngAfterRandom) = phaseRandom initialPRNG afterMax nRandom
        -- Phase 3: minimum-only until no expandable leaves remain (random tie-breaking)
    in phaseMinimum prngAfterRandom afterRandom
  where
    phaseRandom prng tree 0 = (tree, prng)
    phaseRandom prng tree k =
        let expandables = getExpandableNodes tree
        in if null expandables
           then (tree, prng)
           else let (newTree, newPRNG) = execRandom prng (replaceLeafWithNonTerminatingExpansionRandom tree grammar)
                in phaseRandom newPRNG newTree (k - 1)

    phaseMinimum prng tree =
        let expandables = getExpandableNodes tree
        in if null expandables
           then tree
           else let (newTree, newPRNG) = execRandom prng (replaceMinimumCostLeafWithExpansionRandom tree grammar)
                in phaseMinimum newPRNG newTree


constructParseTreeRnd :: Grammar' -> Rnd ParseTree
constructParseTreeRnd (Grammar' grammar minNT nRandom) = do
    let startSym = startingPoint grammar
        initialTree = Leaf startSym []
        afterMax = phaseMaximum grammar minNT initialTree
    afterRandom <- phaseRandomRnd afterMax nRandom
    phaseMinimumRnd afterRandom
  where
    phaseRandomRnd tree 0 = return tree
    phaseRandomRnd tree k = do
        let expandables = getExpandableNodes tree
        if null expandables
           then return tree
           else do newTree <- replaceLeafWithNonTerminatingExpansionRandom tree grammar
                   phaseRandomRnd newTree (k - 1)

    phaseMinimumRnd tree = do
        let expandables = getExpandableNodes tree
        if null expandables
           then return tree
           else do newTree <- replaceMinimumCostLeafWithExpansionRandom tree grammar
                   phaseMinimumRnd newTree

phaseMaximum :: Grammar -> Int -> ParseTree -> ParseTree
phaseMaximum grammar minNT tree =
    let expandables = getExpandableNodes tree
        count = length expandables
    in if null expandables || count >= minNT
       then tree
       else let newTree = replaceMaximumCostLeafWithExpansion tree grammar
                newCount = length (getExpandableNodes newTree)
            in if newCount <= count
               then newTree
               else phaseMaximum grammar minNT newTree

nextGrammarFuzzedString :: Grammar' -> Rnd GrammarFuzzedString
nextGrammarFuzzedString g = do
  tree <- constructParseTreeRnd g
  return $ GrammarFuzzedString (evaluateParseTree tree)

grammarArithmetic :: Grammar
grammarArithmetic = Grammar
  [ (N "Expr",  [ [s (N "Term")]
                , [s (N "Expr"), s (T " + "), s (N "Term")]
                ])
  , (N "Term",  [ [s (N "Factor")]
                , [s (N "Term"), s (T " * "), s (N "Factor")]
                ])
  , (N "Factor", [ [s (N "Number")]
                 , [s (T "("), s (N "Expr"), s (T ")")]
                 ])
  , (N "Number", [ [s (N "Digit"), s (N "Number")]
                 , [s (N "Digit")]
                 ])
  , (N "Digit",  [ [s (T "0")], [s (T "1")], [s (T "2")], [s (T "3")], [s (T "4")]
                 , [s (T "5")], [s (T "6")], [s (T "7")], [s (T "8")], [s (T "9")]
                 ])
  ] (N "Expr")

instance Arbitrary GrammarFuzzedString (Grammar,Int,Int) where
  arbitrary (grammar, minimumNonTerminals, randomExpansions) = nextGrammarFuzzedString (Grammar' grammar minimumNonTerminals randomExpansions)

instance Shrinkable GrammarFuzzedString where
  shrink (GrammarFuzzedString s) = map GrammarFuzzedString (shrink s)


evaluateParseTree :: ParseTree -> String
evaluateParseTree (Tree _ children) = foldl (++) "" $ map evaluateParseTree children
evaluateParseTree (Leaf leafValue _) = show leafValue

prettyPrint :: ParseTree -> String
prettyPrint parseTree = prettyPrint' parseTree 0

prettyPrint' :: ParseTree -> Int -> String
prettyPrint' parseTree@(Tree parent children) depth = let childrenPrettyPrints = concat $ map (\child-> (prettyPrint' child (depth+4)))  children
                                                        in  ( (replicate depth ' ') ++ (show parent) ++ "\n" ) ++ childrenPrettyPrints
prettyPrint' parseTree@(Leaf val path) depth = (replicate depth ' ') ++ show val  ++ "\n"   



getEventuallyTerminatingNonTerminals :: Grammar -> [Symbol]-> [Symbol]
getEventuallyTerminatingNonTerminals grammar found = let newlyFound = filter (\nt -> checkIfInstantlyTerminates' grammar nt found)    ((getDeclaredNonTerminals grammar) \\ found ) 
                                                             in if null newlyFound then found else getEventuallyTerminatingNonTerminals grammar (found++newlyFound)

getTrulyNonTerminatingTerminals :: Grammar -> [Symbol]
getTrulyNonTerminatingTerminals grammar = (getUsedNonTerminals grammar )\\ getEventuallyTerminatingNonTerminals grammar (getInstantlyTerminatingNonTerminals grammar)

getInstantlyTerminatingNonTerminals :: Grammar -> [Symbol]
getInstantlyTerminatingNonTerminals grammar@(Grammar productionRules  _) = filter (\nt -> checkIfInstantlyTerminates grammar nt) (getDeclaredNonTerminals grammar)

isTerminal :: Symbol -> Bool
isTerminal (T _) = True
isTerminal (N _) = False

checkIfInstantlyTerminates' :: Grammar -> Symbol -> [Symbol] -> Bool
checkIfInstantlyTerminates' grammar nt found =
  let expansions = getExpansionsForNonTerminal grammar nt
  in any (all (\(sym, Range lo _) -> isTerminal sym || lo == 0 || sym `elem` found)) expansions

checkIfInstantlyTerminates :: Grammar -> Symbol -> Bool
checkIfInstantlyTerminates grammar nt =
  let expansions = getExpansionsForNonTerminal grammar nt
  in any (all (\(sym, Range lo _) -> isTerminal sym || lo == 0)) expansions
          

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



showEntry :: SymbolEntry -> String
showEntry (sym, Range 1 1) = show sym
showEntry (sym, Range lo hi) = show sym ++ "{" ++ show lo ++ "," ++ show hi ++ "}"

showExpansion :: Expansion -> String
showExpansion expansion = intercalate " " $ map showEntry expansion

showExpansions :: [Expansion] -> String
showExpansions expansions = intercalate "|" (map showExpansion expansions)


-- exp1 = [N "A", T "abc", T "adbbdajk"]
-- expstr = showExpansion exp1

-- exps = [[N "A", T "abc", T "adbbdajk"], [N "A", T "abc", T "adbbdajk"], [N "A", T "abc", T "adbbdajk"]]
-- expsstr = showExpansions exps

showDerivationForNonTerminal:: DerivationsForNonTerminal -> String
showDerivationForNonTerminal (nt, expansions) =  show nt ++ ":" ++ (showExpansions expansions)

showGrammar :: Grammar -> String
showGrammar (Grammar rules start) = intercalate ['\n'] $ map showDerivationForNonTerminal rules





-- creates a folder
createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

-- creates a text file
createTextFile :: FilePath -> String -> IO ()
createTextFile = writeFile

baseDirPath :: FilePath
baseDirPath = "baseDir"


walkFileSystemTreeWrapper :: ParseTree -> IO ()
walkFileSystemTreeWrapper tree = do
  createFolder baseDirPath
  resolved <- canonicalizePath baseDirPath
  putStrLn $ "Building file system under: " ++ resolved
  walkFileSystemTree baseDirPath tree

walkFileSystemTree :: FilePath -> ParseTree -> IO ()
walkFileSystemTree currentPath (Tree parentSym children) = do
  let params = words (show parentSym)
  case params of
    ("D" : name : _) -> do
      let newPath = currentPath </> name
      createFolder newPath
      mapM_ (walkFileSystemTree newPath) children
    _ ->
      mapM_ (walkFileSystemTree currentPath) children

walkFileSystemTree currentPath (Leaf value _) = do
  let params = words (show value)
  case params of
    ("f" : name : rest) ->
      createTextFile (currentPath </> name) (unwords rest)
    ("D" : name : _) ->
      createFolder (currentPath </> name)
    ("E" : name : _) ->
      createFolder (currentPath </> name)
    _ -> return ()
