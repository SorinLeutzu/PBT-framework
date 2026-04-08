module PrettyPrinting.Emojis where

import Data.Map qualified as M

data EmojiName = Cool | Rocket | Happy | Alarm | AllPoints | Correct | Incorrect deriving (Eq, Ord)

emojis =
  M.fromList
    [ (Cool, "😎"),
      (Rocket, "🚀"),
      (Happy,"😄"),
      (Alarm,"🚨"),
      (AllPoints,"💯"),
      (Correct, "✅"),
      (Incorrect, "❌"),
      (Searching, "🔍")
      
    ]

emoji name = case M.lookup name emojis of
  Just code -> code
  Nothing -> ""

data Color = Blue | Red | Green | Purple | Yellow
  deriving (Eq, Ord, Show)

data FontStyle = Bold | Italics | Underline
  deriving (Eq,Ord,Show)
reset :: String
reset = "\ESC[0m"

colorCodes :: M.Map Color String
colorCodes =
  M.fromList
    [ (Blue, "\ESC[34m"),
      (Red, "\ESC[31m"),
      (Green, "\ESC[32m"),
      (Purple, "\ESC[35m"),
      (Yellow, "\ESC[33m")
    ]

colorCode :: Color -> String
colorCode c =
  case M.lookup c colorCodes of
    Just code -> code
    Nothing -> ""


fontStyleCodes :: M.Map FontStyle String
fontStyleCodes =
  M.fromList
    [ (Bold, "\ESC[1m"),
      (Italics, "\ESC[3m"),
      (Underline, "\ESC[4m")
    ]

fontStyleCode :: FontStyle -> String
fontStyleCode c =
  case M.lookup c fontStyleCodes of
    Just code -> code
    Nothing -> ""

colorText :: Color -> String -> String
colorText color text = colorCode color ++ text ++ reset

red :: String -> String
red = colorText Red

blue :: String -> String
blue = colorText Blue

green :: String -> String
green = colorText Green

purple :: String -> String
purple = colorText Purple

stylisedText :: FontStyle -> String -> String
stylisedText fontStyle text = fontStyleCode fontStyle ++ text ++ reset

underline :: String -> String
underline = stylisedText Underline

italics :: String -> String
italics = stylisedText Italics

bold :: String -> String
bold = stylisedText Bold