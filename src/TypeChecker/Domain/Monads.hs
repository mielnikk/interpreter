module TypeChecker.Domain.Monads where

import Control.Monad.Except
import Control.Monad.State
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Error

type EmptyTypeReaderT' = TypeCheckerT' ()

type TypeReaderT = TypeCheckerT' Type

class TypeReader a where
  readType :: a -> TypeReaderT

type TypeCheckerT' a = StateT Environment (Except TypeError) a

type TypeCheckerT = TypeCheckerT' ()

class TypeChecker a where
  checkType :: Maybe Type -> a -> TypeCheckerT
