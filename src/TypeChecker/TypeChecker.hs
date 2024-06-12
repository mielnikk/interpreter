module TypeChecker.TypeChecker where

import Control.Monad.Except
import Control.Monad.State
import Syntax.AbsTortex
import qualified Syntax.Utils as SU
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Error
import qualified TypeChecker.Utils as Utils
import Prelude

instance TypeChecker Program where
  checkType _ (PProgram inits) = do
    Utils.assertUniqueInits inits
    mapM_ (checkType Nothing) inits
    checkType Nothing (SExp (EApp (Ident "main") []))

instance TypeChecker Init where
  checkType _ (IFnDef name arguments returnType block) = do
    Utils.assertValidArguments arguments
    env <- get
    let functionType = SU.calculateFunctionType arguments returnType
    put $ updateEnvironmentType env (name, functionType)
    functionEnv <- get
    let argumentsWithTypes = SU.getArgumentsWithTypes arguments

    put $ updateEnvironmentTypes functionEnv argumentsWithTypes

    checkType (Just returnType) block
    blockEnv <- get
    Utils.assertOrThrow (returnStatementOccuredFlag blockEnv) MissingReturnStatementError
    put functionEnv
    
  checkType _ (IInit name variableType expression) = do
    env <- get
    Utils.assertExpressionType variableType expression
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
      (Just variableType) -> Utils.assertExpressionType variableType expression 
      Nothing -> throwError $ UnknownIdentifierError name

  checkType _ (SIncr name) = do
    Utils.assertVariableType TInt name

  checkType _ (SDecr name) = do
    Utils.assertVariableType TInt name

  checkType (Just expectedType) (SRet expression) = do
    Utils.assertExpressionType expectedType expression
    modify $ updateEnvironmentReturnFlag True

  checkType Nothing (SRet _) = do
    throwError MissingReturnStatementError

  checkType (Just expectedType) SRetVoid = do
    Utils.assertTypesOrThrow expectedType TVoid (InvalidReturnTypeError expectedType TVoid)
    modify $ updateEnvironmentReturnFlag True

  checkType Nothing SRetVoid =
    throwError MissingReturnStatementError

  checkType expected (SCond expression trueBlock) = do
    Utils.assertExpressionType TBool expression
    returnDefinedBefore <- gets returnStatementOccuredFlag
    checkType expected trueBlock
    modify $ updateEnvironmentReturnFlag returnDefinedBefore

  checkType expected (SCondElse expression trueBlock falseBlock) = do
    Utils.assertExpressionType TBool expression
    returnDefinedBefore <- gets returnStatementOccuredFlag
    withStateT (updateEnvironmentReturnFlag False) (checkType expected trueBlock)
    returnTrueBranch <- gets returnStatementOccuredFlag
    withStateT (updateEnvironmentReturnFlag False) (checkType expected falseBlock)
    returnFalseBranch <- gets returnStatementOccuredFlag
    modify $ updateEnvironmentReturnFlag (returnDefinedBefore || (returnTrueBranch && returnFalseBranch))

  checkType expected (SWhile expression block) = do
    Utils.assertExpressionType TBool expression
    checkType expected block

  checkType _ (SExp expression) = do
    Utils.assertExpressionType TVoid expression

instance TypeReader Expr where
  readType (EVar name) =
    Utils.getExistingSymbolOrThrow name (UnknownIdentifierError name)

  readType (ELitInt _) = return TInt

  readType ELitTrue = return TBool

  readType ELitFalse = return TBool

  readType (EString _) = return TStr

  readType (ENeg expression) = do
    Utils.assertExpressionType TInt expression
    return TInt

  readType (ENot expression) = do
    Utils.assertExpressionType TBool expression
    return TBool

  readType (EApp name expressions) = do
    symbolType <- Utils.getExistingSymbolOrThrow name (UnknownIdentifierError name)
    case symbolType of
      (TFun argumentsTypes returnType) -> do
        Utils.assertTypesListOrThrow argumentsTypes expressions
        return returnType
      _ -> throwError (InvalidApplicationError name)

  readType (EMul e1 _ e2) = do
    Utils.assertExpressionTypesOrThrow TInt e1 e2
    return TInt

  readType (EAdd e1 _ e2) = do
    Utils.assertExpressionTypesOrThrow TInt e1 e2
    return TInt

  readType (ERel e1 _ e2) = do
    Utils.assertExpressionTypesOrThrow TInt e1 e2
    return TBool

  readType (EAnd e1 e2) = do
    Utils.assertExpressionTypesOrThrow TBool e1 e2
    return TBool

  readType (EOr e1 e2) = do
    Utils.assertExpressionTypesOrThrow TBool e1 e2
    return TBool

  readType (ELambda arguments returnType block) = do
    Utils.assertValidArguments arguments
    let argumentsWithTypes = SU.getArgumentsWithTypes arguments
    withStateT (`updateEnvironmentTypes` argumentsWithTypes) (checkLambda arguments returnType block)
    where
      checkLambda :: TypeChecker a => [Arg] -> Type -> a -> TypeReaderT
      checkLambda arguments' returnType' block' = do
        let functionType = SU.calculateFunctionType arguments' returnType'
        env <- get
        let result = runExcept (runStateT (Utils.assertValidLambdaBody returnType' block') env)
        case result of
          Left e -> throwError e
          Right _ -> return functionType
