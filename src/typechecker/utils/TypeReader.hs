module TypeChecker.Utils.TypeReader where

import Control.Monad.Except
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.AbsTortex
import TypeChecker.Domain.Monads
import TypeChecker.Error

-- assertTypeOrThrow :: TypeReader a => Type -> a -> a -> EmptyTypeReaderT'
-- assertTypeOrThrow expectedType expr1 expr2 = do
--     assertType expectedType expr1 
--     assertType expectedType expr2 


assertTypeOrThrow :: TypeReader a => Type -> a -> EmptyTypeReaderT'
assertTypeOrThrow expectedType expr = do 
    actualType <- readType expr
    assertOrThrow (expectedType == actualType) MismatchedTypesError

assertOrThrow :: Bool -> TypeError -> EmptyTypeReaderT'
assertOrThrow True _ = pure ()
assertOrThrow False e = throwError e

getExistingSymbolOrThrow :: TypeReader a => a -> m -> TypeReaderT
getExistingSymbolOrThrow name error = asks (left (const error) . lookupIdent name)

