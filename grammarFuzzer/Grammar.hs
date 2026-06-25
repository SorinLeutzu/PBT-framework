{-# LANGUAGE LambdaCase #-}
--{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module GrammarFuzzer.Grammar where

import Random.Core hiding (GrammarFuzzedString)
import Shrinking.Core

import GrammarFuzzer.GrammarDefinitions
import GrammarFuzzer.Cost
import GrammarFuzzer.Expansion
import GrammarFuzzer.Construction
import GrammarFuzzer.Validation
import GrammarFuzzer.Display
import GrammarFuzzer.FileSystem
import GrammarFuzzer.Examples


displayGrammar :: Grammar -> IO()
displayGrammar grammar = putStrLn $ showGrammar grammar



inspectGrammar :: Grammar -> IO()
inspectGrammar grammar = do
  let declaredNonTerminals = getDeclaredNonTerminals grammar
  let usedNonTerminals = getUsedNonTerminals grammar
  let terminals = getTerminals grammar
  putStrLn $ "You have written the following grammar: \n"
  displayGrammar grammar
  let isStartingPointValid = checkStartingPoint grammar
  let word = case isStartingPointValid of
                True -> " is "
                False ->" is not "
  putStrLn $ "Its specified starting symbol is " ++show ( startingPoint grammar) ++ " which " ++ word ++ " valid"
  let flag = declaredNonTerminals == usedNonTerminals
  if flag == True then putStrLn $ "It has the following non terminals: " ++ show declaredNonTerminals
                  else putStrLn $ "There is a mismatch between declared non terminals" ++ show declaredNonTerminals ++ " and the actually used non terminals " ++ show usedNonTerminals
  putStrLn $ "The following non terminals are used " ++ show terminals



main :: IO()
main = do
  let startingTree = Leaf (startingPoint someGrammar) []
  putStrLn $ "starting parse tree: \n" ++ show startingTree
  let newTree = replaceLeafWithExpansion startingTree someGrammar startingTree [ T "ab",T "bc"]
  putStrLn $ "parse tree after 1 expansion \n" ++ show newTree
  let stringFromGrammar =  evaluateParseTree newTree
  putStrLn $ "Grammar fuzzed string is " ++ stringFromGrammar


main' :: IO ()
main' = do
  let prng = PRNG_Xor seedXor
  let startingTree = Leaf (startingPoint someGrammar) []
  let newTree = runRandom prng (replaceLeafWithExpansionRandom startingTree someGrammar)
  putStrLn $ "New tree is " ++ show newTree


main'' :: IO()
main'' = do
  let prng = PRNG_Xor seedXor
  let startingTree = Leaf (startingPoint someGrammar) []
  let newTree = runRandom prng (applyRandomExpansions 10 startingTree someGrammar)
  putStrLn $ "New tree is " ++ show newTree
  putStrLn $ "Pretty print "
  putStrLn $ prettyPrint newTree



getStringFromGrammar :: Grammar' -> String
getStringFromGrammar grammar' = evaluateParseTree $ constructParseTree grammar'


buildFileSystemFromGrammar :: ParseTree -> IO ()
buildFileSystemFromGrammar tree =
  walkFileSystemTreeWrapper tree

main4 :: IO ()
main4 = buildFileSystemFromGrammar simpleFileSystemTree

main3 :: IO()
main3 = do
    let tree = constructParseTree (Grammar' grammarArithmetic' 40 20)
    let string = evaluateParseTree tree
    putStrLn $ string

main7 :: IO()
main7 = do
    let tree = constructParseTree (Grammar' grammarArithmetic' 3 2)
    putStrLn $ prettyPrint tree

main8 :: IO()
main8 = do
    let tree = constructParseTree (Grammar' grammarArithmetic' 3 2)
    let string = evaluateParseTree tree
    putStrLn $ string

instance Arbitrary GrammarFuzzedString (Grammar,Int,Int) where
  arbitrary (grammar, minimumNonTerminals, randomExpansions) = nextGrammarFuzzedString (Grammar' grammar minimumNonTerminals randomExpansions)

instance Shrinkable GrammarFuzzedString where
  shrink (GrammarFuzzedString s) = map GrammarFuzzedString (shrink s)