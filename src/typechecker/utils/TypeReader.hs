module TypeChecker.Utils.TypeReader where

import Control.Monad.Except
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Domain.Monads
import TypeChecker.Error

assertTypeOrThrow :: TypeReader a => Type -> a -> a -> EmptyTypeReaderT'
assertTypeOrThrow expectedType expr1 expr2 = do
    assertTypeOrThrow expectedType expr1 
    assertTypeOrThrow expectedType expr2 

assertType :: TypeReader a => Type -> a -> EmptyTypeReaderT'
assertType expectedType expr = do 
    actualType <- readType expr
    assertOrThrow (expectedType == actualType) MismatchedTypesError

assertOrThrow :: Bool -> TypeError -> EmptyTypeReaderT'
assertOrThrow True _ = pure ()
assertOrThrow False e = throwError e
