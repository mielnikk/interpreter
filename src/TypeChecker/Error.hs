module TypeChecker.Error where

import Syntax.AbsTortex

data TypeError 
    = DuplicatedNameError 
    | MismatchedTypesError 
    | UnknownIdentifierError Ident 
    | InvalidTypeError Type Type 
    | MissingReturnStatementError Ident
    | InvalidReturnTypeError Type Type
    | MissingArgumentError
    | InvalidApplicationError

instance Show TypeError where
    show DuplicatedNameError = "Duplicated name"

    show MismatchedTypesError = "Mismatched types"

    show (UnknownIdentifierError name) = "Unknown identifier: " ++ (show name)

    show (InvalidTypeError expected actual) = "Invalid type - expected: " ++ (show expected) ++ ", actual: " ++ (show actual)

    show (MissingReturnStatementError name) = "Missing return statement in " ++ (show name)

    show (InvalidReturnTypeError actual expected) = "Invalid return type - expected: " ++ show expected ++ ", actual: " ++ show actual

    show MissingArgumentError = "Missing argument in function call"

    show InvalidApplicationError = "Cannot apply"