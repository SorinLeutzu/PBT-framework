module PrettyPrinting.Renders where

import PrettyPrinting.Emojis
import Defs



renderLogEntryPlain :: Show a => (LogEntry a) -> String
renderLogEntryPlain (LogEntry x passed) = "Tried input " ++ show x ++ " - Assertion " ++ (if passed then "passed" else "failed")

renderLogEntry1 :: Show a => (LogEntry a) -> String
renderLogEntry1 (LogEntry x passed) = "Tried input " ++ show x ++ " - Assertion " ++
                       (if passed then green( "passed") ++ emoji Correct else red( "failed") ++ emoji Incorrect)

renderEntryLogsPlain :: Show a => [LogEntry a] -> [String]
renderEntryLogsPlain = map renderLog

renderResultPlain :: Show a => Maybe a -> String
renderResult Nothing = " Matching succeeded "
renderResult (Just minimal) = "Matching failled. Searching for simplest counterexample " ++ show minimal


renderResult1 :: Show a => Maybe a -> String
renderResult Nothing = purple $ " Matching succeeded "
renderResult (Just minimal) = underline "Matching failled" ++ "Searching for simplest counterexample " ++ emoji Searching ++ " " ++ show minimal


renderLog = renderLogEntry1

renderLogs = renderEntryLogsPlain

formatResult = renderResult1