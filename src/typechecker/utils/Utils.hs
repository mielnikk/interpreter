module TypeChecker.Utils where

import TypeChecker.TypeChecker
import TypeChecker.Error
import           Control.Monad.Except


assertTypesOrThrow :: Eq a => a -> a -> TypeError -> TypeCheckerT
assertTypesOrThrow expected actual = assertOrThrow (eqTypes expected actual) where
    eqTypes b b' = b == b'

assertOrThrow :: Bool -> TypeError -> TypeCheckerT
assertOrThrow True _ = pure ()
assertOrThrow False error = throwError error
