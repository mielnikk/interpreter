{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Context where

import Data.Map (Map)
import qualified Data.Map as Map
import Evaluator.Domain.Environment
import Evaluator.Domain.Store
import Syntax.AbsTortex

data Context = Context {environment :: Environment, store :: Store}

emptyContext = Context {environment = emptyEnvironment, store = emptyStore}

getValue :: Ident -> Context -> Value
getValue name Context {..} = getStoreValue (getEnvironmentLocation name environment) store

updateValue :: Ident -> Value -> Context -> Context
updateValue name value Context {..} =
  Context {environment = environment, store = updateStoreValue (getEnvironmentLocation name environment) value store}

insertValue :: Ident -> Value -> Context -> Context
insertValue name value Context {..} =
  Context {environment = newEnv, store = newStore}
  where
    (newLocation, newStore) = insertStoreValue value store
    newEnv = putEnvironmentLocation name newLocation environment

getLocation :: Ident -> Context -> Location
getLocation name Context {..} = getEnvironmentLocation name environment

insertLocation :: Ident -> Location -> Context -> Context
insertLocation name location Context {..} =
  Context {environment = putEnvironmentLocation name location environment, store = store}

insertEnvironment :: Environment -> Context -> Context
insertEnvironment newEnv Context {..} =
  Context {environment = newEnv, store = store}
