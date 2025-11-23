module Emojis where

import Data.Map qualified as M

data EmojiName = Cool | Rocket deriving (Eq, Ord)

emojis =
  M.fromList
    [ (Cool, "😎"),
      (Rocket, "🚀")
    ]

emoji name = case M.lookup name emojis of
  Just code -> code
  Nothing -> ""

data Color = Blue | Red | Green | Purple | Yellow
  deriving (Eq, Ord, Show)

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

colorText :: Color -> String -> String
colorText color text = colorCode color ++ text ++ reset

red :: String -> String
red = colorText Red

blue :: String -> String
blue = colorText Blue

purple :: String -> String
purple = colorText Purple