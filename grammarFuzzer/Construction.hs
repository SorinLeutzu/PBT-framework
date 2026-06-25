module GrammarFuzzer.Construction where

import Random.Core hiding (GrammarFuzzedString)

import GrammarFuzzer.GrammarDefinitions
import GrammarFuzzer.Cost
import GrammarFuzzer.Expansion
import GrammarFuzzer.Display


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