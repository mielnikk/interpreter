module Evaluator.Domain.Value where

import Evaluator.Domain.Environment
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
