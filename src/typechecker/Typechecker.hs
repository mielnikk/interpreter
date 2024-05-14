module TypeChecker.TypeChecker where

import Control.Monad.Except
import Syntax.AbsTortex
import qualified Syntax.Utils as SU
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Error
import qualified TypeChecker.Utils.TypeChecker as TCU
import qualified TypeChecker.Utils.TypeReader as TRU
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
    TCU.assertExpressionTypeOrThrow env variableType expression
    put $ updateEnvironmentType env name variableType


instance TypeChecker Block where
  checkType expectedType (SBlock stmts) = do 
    mapM_ (checkType expectedType) stmts

instance TypeChecker Stmt where
  checkType _ SEmpty = pure ()

  checkType expected (SBStmt block) = do
    env <- get
    checkType _ block
    put env

  checkType expected (SInit init) = checkType expected init

  checkType _ (SAss name expression) = do 
    env <- get
    case (readType env name) of 
      (Just variableType) -> TCU.assertExpressionTypeOrThrow env variableType expression -- TODO: zmienić typ wyjątku
      Nothing -> throwError $ UnknownIdentifierError name

  checkType _ (SIncr name) = TCU.assertVariableTypeOrThrow TInt name

  checkType _ (SDecr name) = TCU.assertVariableTypeOrThrow TInt name
  
  checkType (Just expectedType) (SRet expression) = do 
    env <- get 
    TCU.assertExpressionTypeOrThrow env expectedType expression
    put $ updateEnvironmentReturnFlag env true
  
  checkType Nothing (SRet expression) = do 
    throwError MissingReturnStatementError

  checkType (Just expectedType) SRetVoid = do 
    env <- get
    TCU.assertTypesOrThrow expectedType TVoid (InvalidReturnTypeError expectedType)
    put $ updateEnvironmentReturnFlag env true
  
  checkType Nothing SRetVoid = 
    throwError MissingReturnStatementError
  
  checkType expected (SCond expression trueBlock) = do 
    env <- get
    TCU.assertExpressionTypeOrThrow env TBool expression
    checkType expected trueBlock

  checkType expected (SCondElse expression trueBlock falseBlock) = do 
    env <- get
    TCU.assertExpressionTypeOrThrow env TBool expression
    checkType expected trueBlock
    checkType expected falseBlock
  
  checkType expected (SWhile expression block) = do 
    env <- get
    TCU.assertExpressionTypeOrThrow env TBool expression
    checkType expected block

  checkType _ (SExp expression) = do
    env <- get
    TCU.assertExpressionTypeOrThrow env TVoid expression
    

instance TypeReader Expr where
  readType (EVar name) = 
    TRU.getExistingSymbolOrThrow name pure

  readType (ELitInt _) = pure TInt

  readType ELitTrue = pure TBool

  readType ELitFalse = pure TBool
    
