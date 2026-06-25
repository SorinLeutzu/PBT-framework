module GrammarFuzzer.Validation where

import Data.List

import GrammarFuzzer.GrammarDefinitions


-- performs basic checks on a given grammar
checkGrammar :: Grammar -> Bool
checkGrammar _ = True

getEventuallyTerminatingNonTerminals :: Grammar -> [Symbol]-> [Symbol]
getEventuallyTerminatingNonTerminals grammar found = let newlyFound = filter (\nt -> checkIfInstantlyTerminates' grammar nt found)    ((getDeclaredNonTerminals grammar) \\ found )
                                                             in if null newlyFound then found else getEventuallyTerminatingNonTerminals grammar (found++newlyFound)

getTrulyNonTerminatingTerminals :: Grammar -> [Symbol]
getTrulyNonTerminatingTerminals grammar = (getUsedNonTerminals grammar )\\ getEventuallyTerminatingNonTerminals grammar (getInstantlyTerminatingNonTerminals grammar)

getInstantlyTerminatingNonTerminals :: Grammar -> [Symbol]
getInstantlyTerminatingNonTerminals grammar@(Grammar productionRules  _) = filter (\nt -> checkIfInstantlyTerminates grammar nt) (getDeclaredNonTerminals grammar)

checkIfInstantlyTerminates' :: Grammar -> Symbol -> [Symbol] -> Bool
checkIfInstantlyTerminates' grammar nt found =
  let expansions = getExpansionsForNonTerminal grammar nt
  in any (all (\(sym, Range lo _) -> isTerminal sym || lo == 0 || sym `elem` found)) expansions

checkIfInstantlyTerminates :: Grammar -> Symbol -> Bool
checkIfInstantlyTerminates grammar nt =
  let expansions = getExpansionsForNonTerminal grammar nt
  in any (all (\(sym, Range lo _) -> isTerminal sym || lo == 0)) expansions