module Syntax.Utils where

import Syntax.AbsTortex

getArgumentsWithTypes :: [Arg] -> [(Ident, Type)]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> (Ident, Type)
getArgumentWithType (PArg ident argType) = (ident, argType)
getArgumentWithType (PArgVar ident argType) = (ident, argType)

getArgumentType :: Arg -> Type
getArgumentType (PArg _ argType) = argType
getArgumentType (PArgVar _ argType) = argType

calculateFunctionType :: [Arg] -> Type -> Type
calculateFunctionType args = TFun (map getArgumentType args)
