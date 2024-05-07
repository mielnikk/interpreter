{-# LANGUAGE RecordWildCards #-}
module Typechecker.Environment where
import qualified Data.Map as M
import Syntax.AbsTortex
import Typechecker.Error

data Environment = Environment {types :: M.Map Ident Type, returnType :: Type}

emptyEnvironment = Environment { types = M.fromList builtinMethodsSignatures, returnType = TVoid }

builtinMethodsSignatures :: [(Ident, Type)]
builtinMethodsSignatures = [
  (Ident "printInt", TFun [TInt] TVoid),
  (Ident "printBool", TFun [TBool] TVoid),
  (Ident "printString", TFun [TStr] TVoid)
  ]

data Result = Either TypeError Environment

updateEnvironmentTypes :: Environment -> [(Ident, Type)] -> Environment
updateEnvironmentTypes = foldl updateEnvironmentType

updateEnvironmentType :: Environment -> (Ident, Type) -> Environment
updateEnvironmentType Environment{..} (id, t) = Environment { types = M.insert id t types, returnType = returnType }

updateEnvironmentReturnType :: Environment -> Type -> Environment
updateEnvironmentReturnType Environment{..} t = Environment { types = types, returnType = t }
