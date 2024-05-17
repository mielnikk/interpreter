module Evaluator.Domain.Builtins where

import Control.Monad.Except
import Evaluator.Domain.Context
import Evaluator.Domain.Environment
import Evaluator.Domain.Error
import Evaluator.Domain.Monads
import Syntax.AbsTortex
import Prelude

builtinFunctions = ["printInt", "printBool", "printString"]

isBuiltin :: Ident -> Bool
isBuiltin (Ident name) = name `elem` builtinFunctions

evalBuiltin :: Ident -> [Value] -> EvaluatorT
evalBuiltin _ [value] = do
  liftIO $ putStrLn (show value)
  pure Dummy
evalBuiltin _ _ = throwError $ SomeError
