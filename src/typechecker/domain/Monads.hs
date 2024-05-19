module TypeChecker.Domain.Monads where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Error

type TypeReaderT' a = ReaderT Environment (Except TypeError) a

type EmptyTypeReaderT' = ReaderT Environment (Except TypeError) ()

type TypeReaderT = TypeReaderT' Type

class TypeReader a where
  readType :: a -> TypeReaderT

type TypeCheckerT' a = StateT Environment (Except TypeError) a

type TypeCheckerT = TypeCheckerT' ()

class TypeChecker a where
  checkType :: Maybe Type -> a -> TypeCheckerT
