module Defs where

data LogEntry a = LogEntry
  { leInput :: a,
    lePassed :: Bool
  }
  deriving (Eq, Show)