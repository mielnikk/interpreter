module TypeChecker.TypeChecker where

import TypeChecker.Environment
import TypeChecker.Error
import TypeChecker.Utils
import           Control.Monad.Except
import           Control.Monad.Reader
import Syntax.AbsTortex

type TypeReaderT' a = ReaderT Environment (Except TypeError) a
type EmptyTypeReaderT' a = ReaderT Environment (Except TypeError) a
type TypeReaderT = TypeReaderT' Type

class TypeReader a where 
    readType :: a -> TypeReaderT

type TypeCheckerT' a = StateT Environment (Except TypeError) a
type TypeCheckerT = TypeCheckerT' Type 

class TypeChecker a where
    checkType :: Maybe Type -> a -> TypeCheckerT

instance TypeChecker Program where 
    checkType _ (PProgram inits) = 


