{-# LANGUAGE RecordWildCards #-}

module TypeChecker.Domain.Environment where

import Data.Map (Map)
import qualified Data.Map as Map
import Syntax.AbsTortex

data Environment = Environment {types :: Map Ident Type, returnStatementOccuredFlag :: Bool}

emptyEnvironment :: Environment
emptyEnvironment = Environment {types = Map.fromList builtinMethodsSignatures, returnStatementOccuredFlag = False}

builtinMethodsSignatures :: [(Ident, Type)]
builtinMethodsSignatures =
  [ (Ident "printInt", TFun [TInt] TVoid),
    (Ident "printBool", TFun [TBool] TVoid),
    (Ident "printString", TFun [TStr] TVoid)
  ]

updateEnvironmentTypes :: Environment -> [(Ident, Type)] -> Environment
updateEnvironmentTypes = foldl updateEnvironmentType

updateEnvironmentType :: Environment -> (Ident, Type) -> Environment
updateEnvironmentType Environment {..} (ident, t) = Environment {types = Map.insert ident t types, returnStatementOccuredFlag = returnStatementOccuredFlag}

updateEnvironmentReturnFlag :: Bool -> Environment -> Environment
updateEnvironmentReturnFlag flag Environment {..} = Environment {types = types, returnStatementOccuredFlag = flag}

lookupIdent :: Ident -> Environment -> Maybe Type
lookupIdent ident environment = Map.lookup ident (types environment)
