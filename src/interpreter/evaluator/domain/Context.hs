{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Context where

import Evaluator.Domain.Environment (Environment, Location)
import qualified Evaluator.Domain.Environment as Environment
import Evaluator.Domain.Store (Store)
import qualified Evaluator.Domain.Store as Store
import Evaluator.Domain.Value (Value)
import Syntax.AbsTortex

data Context = Context {environment :: Environment, store :: Store}

empty :: Context
empty = Context {environment = Environment.empty, store = Store.empty}

getValue :: Ident -> Context -> Value
getValue name Context {..} = Store.getValue (Environment.getLocation name environment) store

updateValue :: Ident -> Value -> Context -> Context
updateValue name value Context {..} =
  Context {environment = environment, store = Store.updateValue (Environment.getLocation name environment) value store}

insertValue :: Ident -> Value -> Context -> Context
insertValue name value Context {..} =
  Context {environment = newEnv, store = newStore}
  where
    (location, newStore) = Store.insertValue value store
    newEnv = Environment.insertLocation name location environment

getLocation :: Ident -> Context -> Location
getLocation name Context {..} = Environment.getLocation name environment

insertLocation :: Ident -> Location -> Context -> Context
insertLocation name location Context {..} =
  Context {environment = Environment.insertLocation name location environment, store = store}

insertEnvironment :: Environment -> Context -> Context
insertEnvironment newEnv Context {..} =
  Context {environment = newEnv, store = store}
