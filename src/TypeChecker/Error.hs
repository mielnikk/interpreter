module TypeChecker.Error where

import Syntax.AbsTortex

data TypeError 
    = DuplicatedNameError Ident
    | MismatchedTypesError 
    | UnknownIdentifierError Ident 
    | InvalidTypeError Type Type 
    | MissingReturnStatementError 
    | InvalidReturnTypeError Type Type
    | MissingArgumentError
    | InvalidApplicationError Ident
    | WrongExpressionType Type Type

instance Show TypeError where
    show (DuplicatedNameError name) = "Duplicated name: " ++ show name

    show MismatchedTypesError = "Mismatched types"

    show (UnknownIdentifierError name) = "Unknown identifier: " ++ show name

    show (InvalidTypeError expected actual) = "Invalid type - expected: " ++ show expected ++ ", actual: " ++ show actual

    show MissingReturnStatementError = "Return statement is not defined in all execution branches"

    show (InvalidReturnTypeError actual expected) = "Invalid return type - expected: " ++ show expected ++ ", actual: " ++ show actual

    show MissingArgumentError = "Missing argument in function call"

    show (InvalidApplicationError name) = "Type of " ++ show name ++ " does not allow for application"

    show (WrongExpressionType expected actual) = "Expression should be of type " ++ show expected ++ " but is " ++ show actual
