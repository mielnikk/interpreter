module Evaluator.Utils.Utils where

import Control.Monad.Except
import Control.Monad.State
import Evaluator.Domain.Builtins
import qualified Evaluator.Domain.Context as Context
import Evaluator.Domain.Environment
import Evaluator.Domain.Value
import Evaluator.Domain.Error
import Evaluator.Domain.Monads
import Syntax.AbsTortex
import Prelude

evalIntStmt :: (Integer -> Integer) -> Ident -> EvaluatorT
evalIntStmt f name = do
  ctx <- get
  let value = Context.getValue name ctx
  let newValue = mapVInt f value
  modify $ Context.updateValue name newValue
  return Dummy

evalRollbackEnv :: EvaluatorT -> EvaluatorT
evalRollbackEnv evaluator = do
  ctx <- get
  let env = Context.environment ctx
  _ <- evaluator
  modify $ Context.insertEnvironment env
  return Dummy

evalIntExpr :: Evaluator a => (Integer -> Integer -> Integer) -> a -> a -> EvaluatorT
evalIntExpr f e1 e2 = do
  val1 <- eval e1
  val2 <- eval e2
  return $ mapVInts f val1 val2

evalBoolExpr :: Evaluator a => (Bool -> Bool -> Bool) -> a -> a -> EvaluatorT
evalBoolExpr f e1 e2 = do
  val1 <- eval e1
  val2 <- eval e2
  return $ mapVBools f val1 val2

evalIntBoolExpr :: Evaluator a => (Integer -> Integer -> Bool) -> a -> a -> EvaluatorT
evalIntBoolExpr f e1 e2 = do
  val1 <- eval e1
  val2 <- eval e2
  return $ mapVIntsToVBool f val1 val2

getArgumentLocation :: Expr -> EvaluatorT' Location
getArgumentLocation (EVar name) = gets $ Context.getLocation name
getArgumentLocation _ = pure (-1)

insertArgsToCtx :: (Arg, Value, Location) -> EvaluatorT
insertArgsToCtx (PArg name _, value, _) = do
  modify $ Context.insertValue name value
  return Dummy
insertArgsToCtx (PArgVar _ _, _, -1) =
  throwError InvalidReferenceFunctionApplication
insertArgsToCtx (PArgVar name _, _, location) = do
  modify $ Context.insertLocation name location
  return Dummy

evalWithBuiltinCheck :: Evaluator a => Ident -> [a] -> EvaluatorT -> EvaluatorT
evalWithBuiltinCheck name expressions evaluator = do
  if isBuiltin name
    then do
      argumentVals <- mapM eval expressions
      evalBuiltin name argumentVals
    else evaluator
