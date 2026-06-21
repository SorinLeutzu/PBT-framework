module PrettyPrinting.Renders where

import PrettyPrinting.Emojis
import Defs



renderLogEntryPlain :: Show a => (LogEntry a) -> String
renderLogEntryPlain (LogEntry x passed) = "Tried input " ++ show x ++ " - Assertion " ++ (if passed then "passed" else "failed")

renderLogEntry1 :: Show a => (LogEntry a) -> String
renderLogEntry1 (LogEntry x passed) = "Tried input " ++ show x ++ " - Assertion " ++
                       (if passed then green( "passed") ++ emoji Correct else red( "failed") ++ emoji Incorrect)

renderEntryLogsPlain :: Show a => [LogEntry a] -> [String]
renderEntryLogsPlain = map renderLogEntryPlain

renderResultPlain :: Show a => Maybe a -> String
renderResultPlain Nothing = " Matching succeeded "
renderResultPlain (Just minimal) = "Matching failled. Searching for simplest counterexample " ++ show minimal


renderResult1 :: Show a => Maybe a -> String
renderResult1 Nothing = purple $ " Matching succeeded "
renderResult1 (Just minimal) = underline "Matching failled" ++ "Searching for simplest counterexample " ++ emoji Searching ++ " " ++ show minimal


renderLog :: Show a => LogEntry a -> String
renderLog = renderLogEntry1

renderLogs :: Show a => [LogEntry a] -> [String]
renderLogs = map renderLog

formatResult :: Show a => Maybe a -> String
formatResult = renderResult1