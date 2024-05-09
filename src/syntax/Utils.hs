module Syntax.Utils where

import Syntax.AbsTortex

getArgumentsWithTypes :: [Arg] -> [(Ident, Tyoe)]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> (Ident, Type)
getArgumentWithType (PArg ident argType) = (ident, argType)
getArgumentWithType (PArgVar ident argType _) = (ident, argType)

getArgumentsTypes :: [Arg] -> [Type]
getArgumentsTypes args (PArg _ argType) = argType
getArgumentsTypes (PArgVar _ argType _) = argType

calculateFunctionType :: [Args] -> Type -> Type
calculateFunctionType args = TFun (getArgumentsTypes args)
