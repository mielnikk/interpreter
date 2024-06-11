module Evaluator.Domain.Error where

data EvaluationError
  = UnknownError
  | DivideByZeroError
  | InvalidReferenceFunctionApplication

instance Show EvaluationError where
  show UnknownError = "Unknown error"
  show DivideByZeroError = "Divide by 0"
  show InvalidReferenceFunctionApplication = "Applied value has no location"
