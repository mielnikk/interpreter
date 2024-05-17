module Evaluator.Domain.Error where

import Syntax.AbsTortex
import Evaluator.Domain.Environment(Value)

data EvaluationError
  =  ReturnCalled Value | SomeError | DivideByZeroError

instance Show EvaluationError where
  show SomeError = "Unknown error"
  show DivideByZeroError = "Divide by 0"
