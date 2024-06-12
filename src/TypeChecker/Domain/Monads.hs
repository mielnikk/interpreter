module TypeChecker.Domain.Monads where

import Control.Monad.Except
import Control.Monad.State
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Error

type TypeCheckerT a = StateT Environment (Except TypeError) a

type TypeReaderT = TypeCheckerT Type

class TypeReader a where
  readType :: a -> TypeCheckerT Type

class TypeChecker a where
  checkType :: Maybe Type -> a -> TypeCheckerT ()
