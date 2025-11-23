-- {-# LANGUAGE CPP #-}

module TerminalSetup
  ( setupUtf8Only,
    setConsoleCpUtf8,
    setupTerminalWithChcp,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEncoding, stdout, utf8)
import System.Info (os)
import System.Process (callCommand)

setupUtf8Only :: IO ()
setupUtf8Only = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hFlush stdout

setConsoleCpUtf8 :: IO Bool
setConsoleCpUtf8 =
  if os == "mingw32" || os == "cygwin"
    then do
      res <- try (callCommand "chcp 65001 > nul") :: IO (Either SomeException ())
      case res of
        Right () -> return True
        Left _ -> return False
    else return False

setupTerminalWithChcp :: IO Bool
setupTerminalWithChcp = do
  setupUtf8Only
  ok <- setConsoleCpUtf8
  when (not ok && (os == "mingw32" || os == "cygwin")) $
    putStrLn "Warning: could not set console code page to UTF-8 (chcp failed)."
  return ok
