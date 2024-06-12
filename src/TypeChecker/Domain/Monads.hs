module TypeChecker.Domain.Monads where

import Control.Monad.Except
import Control.Monad.State
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Domain.RawType
import TypeChecker.Error

type TypeCheckerT a = StateT Environment (Except TypeError) a

type TypeReaderT = TypeCheckerT RawType

class HasPosition a => TypeReader a where
  readType :: a -> TypeCheckerT RawType

class TypeChecker a where
  checkType :: Maybe RawType -> a -> TypeCheckerT ()
