module Evaluator.Domain.Builtins where

import Control.Monad.Except
import Evaluator.Domain.Error
import Evaluator.Domain.Monads
import Evaluator.Domain.Value
import Syntax.AbsTortex
import Prelude

builtinFunctions :: [String]
builtinFunctions = ["printInt", "printBool", "printString"]

isBuiltin :: Ident -> Bool
isBuiltin (Ident name) = name `elem` builtinFunctions

evalBuiltin :: Ident -> [Value] -> BNFC'Position -> EvaluatorT
evalBuiltin _ [value] _ = do
  liftIO $ putStrLn (show value)
  pure Dummy
evalBuiltin _ _ position = throwError $ UnknownError position
