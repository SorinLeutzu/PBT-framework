module GrammarFuzzer.Display where

import Data.List

import GrammarFuzzer.GrammarDefinitions


evaluateParseTree :: ParseTree -> String
evaluateParseTree (Tree _ children) = foldl (++) "" $ map evaluateParseTree children
evaluateParseTree (Leaf leafValue _) = show leafValue

prettyPrintIndented :: ParseTree -> String
prettyPrintIndented parseTree = prettyPrintIndented' parseTree 0

prettyPrintIndented' :: ParseTree -> Int -> String
prettyPrintIndented' (Tree parent children) depth =
  (replicate depth ' ' ++ show parent ++ "\n") ++
  concatMap (\child -> prettyPrintIndented' child (depth + 4)) children
prettyPrintIndented' (Leaf val _) depth =
  replicate depth ' ' ++ show val ++ "\n"

prettyPrint :: ParseTree -> String
prettyPrint node = nodeLabel node ++ "\n" ++ renderChildren "" (nodeChildren node)
  where
    nodeLabel    (Tree sym _)  = "[" ++ show sym ++ "]"
    nodeLabel    (Leaf sym _)  = show sym
    nodeChildren (Tree _ cs)   = cs
    nodeChildren (Leaf _ _)    = []

    renderChildren prefix kids =
      concatMap (renderChild prefix (length kids)) (zip [0..] kids)

    renderChild prefix total (i, child) =
      let isLast     = i == total - 1
          connector  = if isLast then "\x2514\x2500\x2500 " else "\x251C\x2500\x2500 " -- └ ─ ├ │
          extension  = if isLast then "    "                 else "\x2502   "
          header     = prefix ++ connector ++ nodeLabel child ++ "\n"
          grandkids  = nodeChildren child
      in header ++ renderChildren (prefix ++ extension) grandkids



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