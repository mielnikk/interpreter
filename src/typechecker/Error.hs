module TypeChecker.Error where

import Syntax.AbsTortex

data TypeError 
    = DuplicatedNameError 
    | MismatchedTypesError 
    | UnknownIdentifierError Ident 
    | InvalidTypeError Type Type 
    | MissingReturnStatementError 
    | InvalidReturnTypeError Type
