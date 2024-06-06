module TypeChecker.TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
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
    TCU.assertUniqueInitsOrThrow inits
    mapM_ (checkType Nothing) inits
    checkType Nothing (SExp (EApp (Ident "main") []))

instance TypeChecker Init where
  checkType _ (IFnDef name arguments returnType block) = do
    TCU.assertValidArgumentsOrThrow arguments
    env <- get
    let functionType = SU.calculateFunctionType arguments returnType
    put $ updateEnvironmentType env (name, functionType)
    functionEnv <- get
    let argumentsWithTypes = SU.getArgumentsWithTypes arguments

    put $ updateEnvironmentTypes functionEnv argumentsWithTypes
    checkType (Just returnType) block
    blockEnv <- get
    TCU.assertOrThrow (returnStatementOccuredFlag blockEnv) (MissingReturnStatementError name)
    put functionEnv
    
  checkType _ (IInit name variableType expression) = do
    env <- get
    TCU.assertExpressionTypeOrThrow env variableType expression
    put $ updateEnvironmentType env (name, variableType)

instance TypeChecker Block where
  checkType expectedType (SBlock stmts) = do
    mapM_ (checkType expectedType) stmts

instance TypeChecker Stmt where
  checkType _ SEmpty = return ()

  checkType expected (SBStmt block) = do
    env <- get
    checkType expected block
    put env

  checkType expected (SInit i) = checkType expected i

  checkType _ (SAss name expression) = do
    env <- get
    case lookupIdent name env of
      (Just variableType) -> TCU.assertExpressionTypeOrThrow env variableType expression 
      Nothing -> throwError $ UnknownIdentifierError name

  checkType _ (SIncr name) = do
    TCU.assertVariableTypeOrThrow TInt name

  checkType _ (SDecr name) = do
    TCU.assertVariableTypeOrThrow TInt name

  checkType (Just expectedType) (SRet expression) = do
    env <- get
    TCU.assertExpressionTypeOrThrow env expectedType expression
    put $ updateEnvironmentReturnFlag env True

  checkType Nothing (SRet _) = do
    throwError MissingReturnStatementError

  checkType (Just expectedType) SRetVoid = do
    env <- get
    TCU.assertTypesOrThrow expectedType TVoid (InvalidReturnTypeError expectedType TVoid)
    put $ updateEnvironmentReturnFlag env True

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
    TRU.getExistingSymbolOrThrow name (UnknownIdentifierError name)

  readType (ELitInt _) = return TInt

  readType ELitTrue = return TBool

  readType ELitFalse = return TBool

  readType (EString _) = return TStr

  readType (ENeg expression) = do
    TRU.assertTypeOrThrow TInt expression
    return TInt

  readType (ENot expression) = do
    TRU.assertTypeOrThrow TBool expression
    return TBool

  readType (EApp name expressions) = do
    symbolType <- TRU.getExistingSymbolOrThrow name (UnknownIdentifierError name)
    case symbolType of
      (TFun argumentsTypes returnType) -> do
        TRU.assertTypesListOrThrow argumentsTypes expressions
        return returnType
      _ -> throwError InvalidApplicationError

  readType (EMul e1 _ e2) = do
    TRU.assertTypesOrThrow TInt e1 e2
    return TInt

  readType (EAdd e1 _ e2) = do
    TRU.assertTypesOrThrow TInt e1 e2
    return TInt

  readType (ERel e1 _ e2) = do
    TRU.assertTypesOrThrow TInt e1 e2
    return TBool

  readType (EAnd e1 e2) = do
    TRU.assertTypesOrThrow TBool e1 e2
    return TBool

  readType (EOr e1 e2) = do
    TRU.assertTypesOrThrow TBool e1 e2
    return TBool

  readType (ELambda arguments returnType block) = do
    TRU.assertValidArgumentsOrThrow arguments
    let argumentsWithTypes = SU.getArgumentsWithTypes arguments
    local (`updateEnvironmentTypes` argumentsWithTypes) (checkLambda arguments returnType block)
    where
      checkLambda :: TypeChecker a => [Arg] -> Type -> a -> TypeReaderT
      checkLambda arguments' returnType' block' = do
        let functionType = SU.calculateFunctionType arguments' returnType'
        env <- ask
        let result = runExcept (runStateT (TCU.assertValidLambdaBodyOrThrow returnType' block') env)
        case result of
          Left e -> throwError e
          Right _ -> return functionType
