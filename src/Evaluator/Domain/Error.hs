module Evaluator.Domain.Error where

import Evaluator.Domain.Value(Value)

data EvaluationError
  =  ReturnCalled Value | UnknownError | DivideByZeroError | InvalidReferenceFunctionApplication

instance Show EvaluationError where
  show UnknownError = "Unknown error"
  show DivideByZeroError = "Divide by 0"
