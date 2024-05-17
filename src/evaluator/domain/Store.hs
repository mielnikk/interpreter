{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Store where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Evaluator.Domain.Environment

data Store = Store {_store :: Map Location Value, newLocation :: Location}

emptyStore = Store {_store = Map.empty, newLocation = 0}

getStoreValue :: Location -> Store -> Value
getStoreValue location Store {..} = fromJust $ Map.lookup location _store

updateStoreValue :: Location -> Value -> Store -> Store
updateStoreValue location value Store {..} =
  Store {_store = Map.insert location value _store, newLocation = newLocation}

insertStoreValue :: Value -> Store -> (Location, Store)
insertStoreValue value Store {..} =
  (newLocation, Store {_store = newStore, newLocation = nextLocation})
  where
    nextLocation = newLocation + 1
    newStore = Map.insert newLocation value _store
