{-# LANGUAGE RecordWildCards #-}

module TypeChecker.Domain.Environment where

import Data.Map (Map)
import qualified Data.Map as Map
import Syntax.AbsTortex
import TypeChecker.Domain.RawType

data Environment = Environment {types :: Map Ident RawType, returnStatementOccuredFlag :: Bool}

emptyEnvironment :: Environment
emptyEnvironment = Environment {types = Map.fromList builtinMethodsSignatures, returnStatementOccuredFlag = False}

builtinMethodsSignatures :: [(Ident, RawType)]
builtinMethodsSignatures =
  [ (Ident "printInt", RTFun [RTInt] RTVoid),
    (Ident "printBool", RTFun [RTBool] RTVoid),
    (Ident "printString", RTFun [RTString] RTVoid)
  ]

updateEnvironmentTypes :: Environment -> [(Ident, RawType)] -> Environment
updateEnvironmentTypes = foldl updateEnvironmentType

updateEnvironmentType :: Environment -> (Ident, RawType) -> Environment
updateEnvironmentType Environment {..} (ident, t) = Environment {types = Map.insert ident t types, returnStatementOccuredFlag = returnStatementOccuredFlag}

updateEnvironmentReturnFlag :: Bool -> Environment -> Environment
updateEnvironmentReturnFlag flag Environment {..} = Environment {types = types, returnStatementOccuredFlag = flag}

lookupIdent :: Ident -> Environment -> Maybe RawType
lookupIdent ident environment = Map.lookup ident (types environment)
