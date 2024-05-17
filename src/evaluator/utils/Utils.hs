module Evaluator.Utils.Utils where

import Control.Monad.State
import Evaluator.Domain.Context
import Evaluator.Domain.Environment
import Evaluator.Domain.Monads
import Syntax.AbsTortex
import Prelude

evalIntStmt :: (Integer -> Integer) -> Ident -> EvaluatorT
evalIntStmt f name = do
  ctx <- get
  let value = getValue name ctx
  let newValue = mapVInt f value
  modify $ updateValue name newValue
  return Dummy

keepEnvAndEval :: EvaluatorT -> EvaluatorT
keepEnvAndEval exec = do
  ctx <- get
  let env = environment ctx
  _ <- exec
  modify $ putEnv env
  return Dummy

evalIntExpr :: Evaluator a => (Integer -> Integer -> Integer) -> a -> a -> EvaluatorT
evalIntExpr f e1 e2 = do
  val1 <- eval e1
  val2 <- eval e2
  pure $ mapVInts f val1 val2

evalBoolExpr :: Evaluator a => (Bool -> Bool -> Bool) -> a -> a -> EvaluatorT
evalBoolExpr f e1 e2 = do
  val1 <- eval e1
  val2 <- eval e2
  pure $ mapVBools f val1 val2

evalIntBoolExpr :: Evaluator a => (Integer -> Integer -> Bool) -> a -> a -> EvaluatorT
evalIntBoolExpr f e1 e2 = do
  val1 <- eval e1
  val2 <- eval e2
  pure $ mapVIntsToVBool f val1 val2
