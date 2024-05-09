module TypeChecker.TypeChecker where

import Control.Monad.Except
import Syntax.AbsTortex
import TypeChecker.Environment
import TypeChecker.Error
import Prelude 
import TypeChecker.Utils.Monads
import TypeChecker.Utils.Utils

instance TypeChecker Program where
  checkType _ (PProgram inits) = do
    assertUniqueInitsOrThrow inits
    mapM_ (checkType Nothing) inits
    checkType Nothing (SExp (EApp (Ident "main") []))
