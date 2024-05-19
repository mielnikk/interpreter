module Evaluator.Domain.Monads where

import Control.Monad.Except
import Control.Monad.State
import Evaluator.Domain.Context
import Evaluator.Domain.Error
import Evaluator.Domain.Value (Value)
import Prelude

type EvaluatorT = EvaluatorT' Value

type EvaluatorT' a = StateT Context (ExceptT EvaluationError IO) a

class Evaluator a where
  eval :: a -> EvaluatorT
