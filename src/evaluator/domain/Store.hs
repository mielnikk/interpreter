{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Store where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Evaluator.Domain.Environment

data Store = Store {_store :: Map Location Value, currentLocation :: Location}

emptyStore = Store {_store = Map.empty, currentLocation = 0}

getStoreValue :: Location -> Store -> Value
getStoreValue location Store {..} = fromJust $ Map.lookup location _store

updateStoreValue :: Location -> Value -> Store -> Store
updateStoreValue location value Store {..} =
  Store {_store = Map.insert location value _store, currentLocation = currentLocation}

insertStoreValue :: Value -> Store -> (Location, Store)
insertStoreValue value Store {..} =
  (nextLocation, Store {_store = newStore, currentLocation = nextLocation})
  where
    nextLocation = currentLocation + 1
    newStore = Map.insert nextLocation value _store
