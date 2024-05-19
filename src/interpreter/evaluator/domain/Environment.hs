{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Environment where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Syntax.AbsTortex

type Location = Int

newtype Environment = Environment {_environment :: Map Ident Location}

empty :: Environment
empty = Environment {_environment = Map.empty}

getLocation :: Ident -> Environment -> Location
getLocation name Environment {..} = fromJust $ Map.lookup name _environment

insertLocation :: Ident -> Location -> Environment -> Environment
insertLocation name location Environment {..} =
  Environment {_environment = Map.insert name location _environment}
