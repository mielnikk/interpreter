module TypeChecker.Domain.RawType where

import Syntax.AbsTortex

data RawType
  = RTInt
  | RTString
  | RTBool
  | RTVoid
  | RTFun [RawType] RawType
  deriving (Eq)

instance Show RawType where
  show RTInt = "Int"
  show RTString = "String"
  show RTBool = "Bool"
  show RTVoid = "Void"
  show (RTFun argsTypes returnType) =
    concat
      [ "(",
        show argsTypes,
        ")",
        " -> ",
        show returnType
      ]

fromType :: Type -> RawType
fromType (TInt _) = RTInt
fromType (TStr _) = RTString
fromType (TBool _) = RTBool
fromType (TVoid _) = RTVoid
fromType (TFun _ argumentsTypes returnType) = RTFun rawArgumentsTypes rawReturnType
  where
    rawArgumentsTypes = reduceVoidToEmptyList $ map fromType argumentsTypes
    rawReturnType = fromType returnType

getArgumentType :: Arg -> RawType
getArgumentType (PArg _ _ argType) = fromType argType
getArgumentType (PArgVar _ _ argType) = fromType argType

getArgumentsWithTypes :: [Arg] -> [(Ident, RawType)]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> (Ident, RawType)
getArgumentWithType (PArg _ ident argType) = (ident, fromType argType)
getArgumentWithType (PArgVar _ ident argType) = (ident, fromType argType)

calculateFunctionType :: [Arg] -> Type -> RawType
calculateFunctionType args = RTFun (reduceVoidToEmptyList (map getArgumentType args)) . fromType 

reduceVoidToEmptyList :: [RawType] -> [RawType]
reduceVoidToEmptyList [RTVoid] = []
reduceVoidToEmptyList x = x
