{-# LANGUAGE FlexibleInstances #-}

module TypeChecker.Error where

import Syntax.AbsTortex
import TypeChecker.Domain.RawType

data TypeError' a
  = DuplicatedNameError a Ident
  | UnknownIdentifierError a Ident
  | InvalidTypeError a RawType RawType
  | MissingReturnStatementError a
  | InvalidReturnTypeError a RawType RawType
  | MissingArgumentError a
  | InvalidApplicationError a Ident
  | WrongExpressionType a RawType RawType

type TypeError = TypeError' BNFC'Position

instance Show TypeError where
  show (DuplicatedNameError pos name) =
    errorAt pos ++ "Duplicated name: " ++ show name
  show (UnknownIdentifierError pos name) =
    errorAt pos ++ "Unknown identifier: " ++ show name
  show (InvalidTypeError pos expected actual) =
    errorAt pos ++ "Invalid type - expected: " ++ show expected ++ ", actual: " ++ show actual
  show (MissingReturnStatementError pos) =
    errorAt pos ++ "Return statement is not defined in all execution branches"
  show (InvalidReturnTypeError pos actual expected) =
    errorAt pos ++ "Invalid return type - expected: " ++ show expected ++ ", actual: " ++ show actual
  show (MissingArgumentError pos) =
    errorAt pos ++ "Missing argument in function call"
  show (InvalidApplicationError pos name) =
    errorAt pos ++ "Type of " ++ show name ++ " does not allow for application"
  show (WrongExpressionType pos expected actual) =
    errorAt pos ++ "Expression should be of type " ++ show expected ++ " but is " ++ show actual

errorAt :: BNFC'Position -> String
errorAt pos = "ERROR at " ++ showPos pos ++ ": "

showPos :: BNFC'Position -> String
showPos (Just (line, column)) = concat ["line ", show line, ", column ", show column]
showPos _ = "unknown"
