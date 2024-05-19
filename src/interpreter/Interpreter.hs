module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Evaluator.Evaluator
import Lexer.ParTortex (myLexer, pProgram)
import Syntax.AbsTortex
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, stderr)
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.TypeChecker()
import Prelude (Either (Left, Right), FilePath, IO, Maybe (Nothing), String, pure, readFile)

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpret

interpret :: String -> IO ()
interpret code = do
  program <- runLexer code
  _ <- runTypeCheck program
  _ <- runEvaluate program
  exitSuccess

runLexer :: String -> IO Program
runLexer code = case pProgram (myLexer code) of
  Left err -> hPrint stderr err >> exitFailure
  Right x -> return x

runTypeCheck :: Program -> IO ()
runTypeCheck program = case runExcept (runStateT (checkType Nothing program) emptyEnvironment) of
  Right _ -> pure ()
  Left err -> hPrint stderr err >> exitFailure
