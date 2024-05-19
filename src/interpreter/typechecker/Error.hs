module TypeChecker.Error where

import Syntax.AbsTortex

data TypeError 
    = DuplicatedNameError Ident  
    | MismatchedTypeError Type 
    | UnknownIdentifierError Ident 
    | InvalidTypeError Type Type 
    | MissingReturnStatementError 
    | InvalidReturnTypeError Type Type
    | MissingArgumentError
    | InvalidApplicationError

instance Show TypeError where
    show (DuplicatedNameError name) = "Duplicated name: " ++ show name

    show (MismatchedTypeError actual) = "Mismatched  types: operator does not apply to " ++ show actual

    show (UnknownIdentifierError name) = "Unknown identifier: " ++ show name

    show (InvalidTypeError expected actual) = "Invalid type - expected: " ++ show expected ++ ", actual: " ++ show actual

    show MissingReturnStatementError = "Missing return statement"

    show (InvalidReturnTypeError actual expected) = "Invalid return type - expected: " ++ show expected ++ ", actual: " ++ show actual

    show MissingArgumentError = "Missing argument in function call"

    show InvalidApplicationError = "Cannot apply"