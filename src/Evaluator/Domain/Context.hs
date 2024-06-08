{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Context where

import Evaluator.Domain.Environment (Environment, Location)
import qualified Evaluator.Domain.Environment as Environment
import Evaluator.Domain.Store (Store)
import qualified Evaluator.Domain.Store as Store
import Evaluator.Domain.Value (Value (Dummy))
import Syntax.AbsTortex

data Context = Context {environment :: Environment, store :: Store, returnFlag :: Bool}

empty :: Context
empty =
  let ctx = Context {environment = Environment.empty, store = Store.empty, returnFlag = False}
   in insertValue returnLabel Dummy ctx

getValue :: Ident -> Context -> Value
getValue name Context {..} = Store.getValue (Environment.getLocation name environment) store

updateValue :: Ident -> Value -> Context -> Context
updateValue name value Context {..} =
  Context {environment = environment, store = Store.updateValue (Environment.getLocation name environment) value store, returnFlag = returnFlag}

insertValue :: Ident -> Value -> Context -> Context
insertValue name value Context {..} =
  Context {environment = newEnv, store = newStore, returnFlag = returnFlag}
  where
    (location, newStore) = Store.insertValue value store
    newEnv = Environment.insertLocation name location environment

getLocation :: Ident -> Context -> Location
getLocation name Context {..} = Environment.getLocation name environment

insertLocation :: Ident -> Location -> Context -> Context
insertLocation name location Context {..} =
  Context {environment = Environment.insertLocation name location environment, store = store, returnFlag = returnFlag}

insertEnvironment :: Environment -> Context -> Context
insertEnvironment newEnv Context {..} =
  Context {environment = newEnv, store = store, returnFlag = returnFlag}

setReturnFlag :: Bool -> Context -> Context
setReturnFlag value Context {..} = Context {environment = environment, store = store, returnFlag = value}

returnLabel :: Ident
returnLabel = Ident "_return"

setReturned :: Value -> Context -> Context
setReturned value = setReturnFlag True . insertValue returnLabel value

getReturnValue :: Context -> Value
getReturnValue = getValue returnLabel
