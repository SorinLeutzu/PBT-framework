module PrettyPrinting.Renders where

import PrettyPrinting.Emojis
import Config (withColors, withEmojis)
import Defs
import Data.List (intercalate)
import System.IO (hFlush, stdout)


-- log entry rendering

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


-- test result line rendering

renderPassLine :: Int -> String -> String -> String
renderPassLine indent name msg =
  indentStr indent ++ passMarker ++ " " ++ condGreen name ++ "\n" ++ indentStr (indent + 2) ++ msg

renderFailLine :: Int -> String -> String -> String
renderFailLine indent name msg =
  indentStr indent ++ failMarker ++ " " ++ condRed name ++ "\n" ++ indentStr (indent + 2) ++ msg

passMarker :: String
passMarker = if withEmojis then emoji Correct else condGreen "[PASS]"

failMarker :: String
failMarker = if withEmojis then emoji Incorrect else condRed "[FAIL]"

renderSuiteHeader :: Int -> String -> String
renderSuiteHeader indent name =
  let bar = replicate 2 '━'
  in indentStr indent ++ condBold (bar ++ " " ++ name ++ " " ++ bar)

indentStr :: Int -> String
indentStr n = replicate n ' '


-- Progress bar

data ProgressBar = ProgressBar
  { pbTotal   :: Int
  , pbCurrent :: Int
  , pbWidth   :: Int
  , pbLabel   :: String
  }

mkProgressBar :: String -> Int -> ProgressBar
mkProgressBar label total = ProgressBar
  { pbTotal   = total
  , pbCurrent = 0
  , pbWidth   = 30
  , pbLabel   = label
  }


renderProgressBar :: ProgressBar -> IO ()
renderProgressBar pb@(ProgressBar total current width label) = do
  let filled = round $ (fromIntegral current) / (fromIntegral total) * (fromIntegral width)
  let toBeFilled = width - filled
  let bar = replicate filled '█' ++ replicate toBeFilled '░'
  let line = "\r " ++ label ++ "[" ++ bar ++ "]" ++ show current ++ "/" ++ show total
  putStr line
  hFlush stdout

updateProgressBar :: ProgressBar -> Int -> IO ProgressBar
updateProgressBar (ProgressBar total current width label) n = do
  let newPb = ProgressBar total n width label
  renderProgressBar $ newPb
  return newPb

finishProgressBar :: ProgressBar -> IO ()
finishProgressBar pb@(ProgressBar total current width label) = do
  let fullPb = ProgressBar total total width label
  renderProgressBar fullPb
  putStrLn ""

-- Spinner

data Spinner = Spinner
  { spFrames :: [String]
  , spIndex  :: Int
  , spLabel  :: String
  }

defaultSpinner :: String -> Spinner
defaultSpinner label = Spinner
  { spFrames = ["⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏"]
  , spIndex  = 0
  , spLabel  = label
  }

renderSpinner :: Spinner -> IO Spinner
renderSpinner spinner@(Spinner frames index label) = do
  let frame = frames !!  ( index `mod` ( length frames))
  let line = "\r " ++ frame ++ label
  putStr line
  hFlush stdout
  return $ Spinner frames (index+1) label  


finishSpinner :: Spinner -> String -> IO()
finishSpinner spinner msg = do
  putStr $ "\r " ++ replicate ((length (spLabel spinner))+10) ' ' ++ "\r"
  hFlush stdout
  putStrLn (" " ++ msg)



condGreen :: String -> String
condGreen s = if withColors then green s else s

condRed :: String -> String
condRed s = if withColors then red s else s

condBold :: String -> String
condBold s = if withColors then bold s else s

condPurple :: String -> String
condPurple s = if withColors then purple s else s