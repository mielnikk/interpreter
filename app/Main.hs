module Main where

import Interpreter
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Prelude (IO, putStrLn, (>>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> interpretFile f
    _ -> putStrLn "Invalid usage!" >> exitFailure
