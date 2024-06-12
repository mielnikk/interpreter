{-# LANGUAGE FlexibleInstances #-}

module Evaluator.Domain.Error where

import Syntax.AbsTortex

data EvaluationError' a
  = UnknownError a
  | DivideByZeroError a
  | InvalidReferenceFunctionApplication a

type EvaluationError = EvaluationError' BNFC'Position

instance Show EvaluationError where
  show (UnknownError pos) = errorAt pos ++ "Unknown error"
  show (DivideByZeroError pos) = errorAt pos ++ "Divide by 0"
  show (InvalidReferenceFunctionApplication pos) = errorAt pos ++ "Applied value has no location"

errorAt :: BNFC'Position -> String
errorAt pos = "ERROR at " ++ showPos pos ++ ": "

showPos :: BNFC'Position -> String
showPos (Just (line, column)) = concat ["line ", show line, ", column ", show column]
showPos _ = "unknown"
