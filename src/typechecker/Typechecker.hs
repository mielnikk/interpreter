module TypeChecker.TypeChecker where

import Control.Monad.Except
import Syntax.AbsTortex
import qualified Syntax.Utils as SU
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Error
import qualified TypeChecker.Utils.Utils as TCU
import Prelude

instance TypeChecker Program where
  checkType _ (PProgram inits) = do
    assertUniqueInitsOrThrow inits
    mapM_ (checkType Nothing) inits
    checkType Nothing (SExp (EApp (Ident "main") []))

instance TypeChecker Init where
  checkType _ (IFnDef name, arguments, returnType, block) = do
    TCU.checkUniqueArguments arguments
    env <- get
    let functionType = SU.calculateFunctionType arguments returnType
    put $ updateEnvironmentType env name functionType
    functionEnv <- get
    let argumentsWithTypes = SU.getArgumentsWithTypes arguments

    put $ updateEnvironmentTypes functionEnv name
    checkType (Just returnType) block
    blockEnv <- get
    -- TODO: check for return statement
    put functionEnv

  checkType _ (IInit name, variableType, expression) = do
    env <-  get
    TCU.

