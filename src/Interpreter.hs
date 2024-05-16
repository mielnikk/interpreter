module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Lexer.ParTortex (myLexer, pProgram)
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.TypeChecker
import Prelude (Either (Left, Right), FilePath, IO, Maybe (Nothing), String, print, readFile, show, ($), (.), (>>=))

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpret

interpret :: String -> IO ()
interpret program = case pProgram (myLexer program) of
  Right p -> runTypeCheck p
  Left _ -> putStrLn "SYNTAX CHUJ"

runTypeCheck :: Program -> IO ()
runTypeCheck program = case runExcept (runStateT (checkType Nothing program) emptyEnvironment) of
  Right _ -> putStrLn "GUT"
  Left e -> print e
