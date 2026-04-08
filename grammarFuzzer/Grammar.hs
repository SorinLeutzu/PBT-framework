{-# LANGUAGE LambdaCase #-}
--{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module GrammarFuzzer.Grammar where

import Data.Containers.ListUtils as L
import Data.Graph
import Data.List

import Random
import Control.Monad (foldM)

import Data.Map qualified as M



import GrammarFuzzer.GrammarDefinitions
import GrammarFuzzer.GrammarHelper


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



someGrammar2 :: Grammar
someGrammar2 = Grammar
  [ (N "A", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "B"), s (T "aaa"), s (N "B")]
           ])
  , (N "B", [ [s (T "ab"), s (T "ffff"), s (T "bc")]
           , [s (N "A"), s (T "aaa")]
           ])
  , (N "C", [ [s (T "ab"), s (T "bc")]
           , [s (N "B"), s (T "aaa")]
           ])
  ] (N "A")

someGrammar3 :: Grammar
someGrammar3 =  Grammar
  [ (N "A", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "B"), s (T "aaa"), s (N "A")]
           ])
  , (N "B", [ [s (T "ab"), s (T "ffff"), s (T "bc")]
           , [s (N "A"), s (T "aaa")]
           , [s (N "A")]
           ])
  , (N "C", [ [s (T "ab"), s (T "bc")]
           , [s (N "B"), s (T "aaa")]
           ])
  , (N "D", [ [s (N "D"), s (N "D")]
           , [s (N "D"), s (T "aaa"), s (N "D"), s (N "D")]
           ])
  ] (N "A")

someGrammar4 :: Grammar
someGrammar4 =  Grammar
  [ (N "A",  [ [s (T "bla")]
           ])
  ] (N "A")


grammarDigits :: Grammar
grammarDigits = Grammar
  [ (N "Digits", [ [s (T "0"), s (N "Digits")], [s (T "1"), s (N "Digits")], [s (T "2"), s (N "Digits")]
                 , [s (T "3"), s (N "Digits")], [s (T "4"), s (N "Digits")], [s (T "5"), s (N "Digits")]
                 , [s (T "6"), s (N "Digits")], [s (T "7"), s (N "Digits")], [s (T "8"), s (N "Digits")], [s (T "9"), s (N "Digits")]
                 , [s (T "0")], [s (T "1")], [s (T "2")], [s (T "3")], [s (T "4")], [s (T "5")], [s (T "6")], [s (T "7")], [s (T "8")], [s (T "9")]
                 ])
  ] (N "Digits")

grammarGreeting :: Grammar
grammarGreeting = Grammar
  [ (N "Greeting", [ [s (T "hi")]
                   , [s (T "hi"), s (T " "), s (N "Greeting")]
                   ])
  ] (N "Greeting")

grammarAB :: Grammar
grammarAB = Grammar
  [ (N "S", [ [s (T "a"), s (N "S")], [s (T "b"), s (N "S")], [s (T "a")], [s (T "b")] ])
  ] (N "S")

grammarParens :: Grammar
grammarParens = Grammar
  [ (N "P", [ [s (T "("), s (T ")"), s (N "P")]
            , [s (T "("), s (N "P"), s (T ")"), s (N "P")]
            , [s (T "("), s (T ")")]
            ])
  ] (N "P")

grammarArithmetic' :: Grammar
grammarArithmetic' = Grammar
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

getStringFromGrammar :: Grammar' -> String
getStringFromGrammar grammar' = evaluateParseTree $ constructParseTree grammar' 


simpleFileSystemTree :: ParseTree
simpleFileSystemTree =
  Tree (T "D root")
    [ Leaf (T "f hello.txt Hello from the fuzzer") []
    , Tree (T "D docs")
        [ Leaf (T "f readme.md # Readme\nThis was generated.") []
        , Leaf (T "E empty") []
        ]
    , Leaf (T "f notes.txt line1\nline2\nline3") []
    ]


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