module Main where

import Interpreter
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Prelude
  ( IO,
    putStrLn,
    unlines,
    ($),
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [f] -> interpretFile f
    _ -> invalidUsage

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (file)         Parse content of the file."
      ]
  exitFailure

invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid usage! Use --help."
  exitFailure
