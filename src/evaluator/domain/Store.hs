{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Store where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Evaluator.Domain.Environment
import Evaluator.Domain.Value (Value)

data Store = Store {_store :: Map Location Value, currentLocation :: Location}

empty :: Store
empty = Store {_store = Map.empty, currentLocation = 0}

getValue :: Location -> Store -> Value
getValue location Store {..} = fromJust $ Map.lookup location _store

updateValue :: Location -> Value -> Store -> Store
updateValue location value Store {..} =
  Store {_store = Map.insert location value _store, currentLocation = currentLocation}

insertValue :: Value -> Store -> (Location, Store)
insertValue value Store {..} =
  (nextLocation, Store {_store = newStore, currentLocation = nextLocation})
  where
    nextLocation = currentLocation + 1
    newStore = Map.insert nextLocation value _store
