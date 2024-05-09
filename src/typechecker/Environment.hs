{-# LANGUAGE RecordWildCards #-}

module TypeChecker.Environment where

import Data.Map (Map)
import qualified Data.Map as Map
import Syntax.AbsTortex
import TypeChecker.Error

data Environment = Environment {types :: Map Ident Type, returnType :: Type}

-- emptyEnvironment = Environment { types = Map.fromList builtinMethodsSignatures, returnType = TVoid }

builtinMethodsSignatures :: [(Ident, Type)]
builtinMethodsSignatures =
  [ (Ident "printInt", TFun [TInt] TVoid),
    (Ident "printBool", TFun [TBool] TVoid),
    (Ident "printString", TFun [TStr] TVoid)
  ]

updateEnvironmentTypes :: Environment -> [(Ident, Type)] -> Environment
updateEnvironmentTypes = foldl updateEnvironmentType

updateEnvironmentType :: Environment -> (Ident, Type) -> Environment
updateEnvironmentType Environment {..} (ident, t) = Environment {types = Map.insert ident t types, returnType = returnType}

updateEnvironmentReturnType :: Environment -> Type -> Environment
updateEnvironmentReturnType Environment {..} t = Environment {types = types, returnType = t}

lookupIdent :: Ident -> Environment -> Maybe Type
lookupIdent ident environment = Map.lookup ident (types environment)
