{-# LANGUAGE RecordWildCards #-}

module Evaluator.Domain.Environment where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Syntax.AbsTortex

data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VFun [Arg] Block Environment
  | VVoid
  | Dummy

instance Show Value where
  show (VInt val) = show val
  show (VBool val) = show val
  show (VString val) = val
  show _ = ""

mapVInt :: (Integer -> Integer) -> Value -> Value
mapVInt f (VInt val) = VInt $ f val

mapVInts :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
mapVInts f (VInt val1) (VInt val2) = VInt $ f val1 val2

mapVIntsToVBool :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
mapVIntsToVBool f (VInt val1) (VInt val2) = VBool $ f val1 val2

mapVBool :: (Bool -> Bool) -> Value -> Value
mapVBool f (VBool val) = VBool $ f val

mapVBools :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
mapVBools f (VBool val1) (VBool val2) = VBool $ f val1 val2

isTrue :: Value -> Bool
isTrue (VBool True) = True
isTrue _ = False

isIntEqual :: Value -> Integer -> Bool
isIntEqual (VInt val) expectedVal = val == expectedVal
isIntEqual _ _ = False

getFunctionEnvironment :: Value -> Environment
getFunctionEnvironment (VFun _ _ env) = env

getFunctionArgs :: Value -> [Arg]
getFunctionArgs (VFun args _ _) = args

getFunctionBlock :: Value -> Block
getFunctionBlock (VFun _ block _) = block

isDummy :: Value -> Bool
isDummy Dummy = True
isDummy _ = False

type Location = Int

newtype Environment = Environment {_environment :: Map Ident Location}

emptyEnvironment = Environment {_environment = Map.empty}

getEnvironmentLocation :: Ident -> Environment -> Location
getEnvironmentLocation name Environment {..} = fromJust $ Map.lookup name _environment

insertEnvironmentLocation :: Ident -> Location -> Environment -> Environment
insertEnvironmentLocation name location Environment {..} =
  Environment {_environment = Map.insert name location _environment}
