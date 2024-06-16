{-# LANGUAGE FlexibleInstances #-}

module TypeChecker.TypeChecker where

import Control.Monad.Except
import Control.Monad.State
import Syntax.AbsTortex
import TypeChecker.Domain.Environment
import TypeChecker.Domain.Monads
import TypeChecker.Domain.RawType
import TypeChecker.Error
import qualified TypeChecker.Utils as Utils
import Prelude

instance TypeChecker Program where
  checkType _ (PProgram _ inits) = do
    Utils.assertUniqueInits inits
    mapM_ (checkType Nothing) inits
    checkType Nothing (SExp BNFC'NoPosition (EApp BNFC'NoPosition (Ident "main") []))

instance TypeChecker Init where
  checkType _ (IFnDef position name arguments returnType block) = do
    Utils.assertValidArguments arguments
    env <- get
    let functionType = calculateFunctionType arguments returnType
    put $ updateEnvironmentType env (name, functionType)
    functionEnv <- get
    let argumentsWithTypes = getArgumentsWithTypes arguments

    put $ updateEnvironmentTypes functionEnv argumentsWithTypes

    checkType (Just $ fromType returnType) block
    blockEnv <- get
    Utils.assertOrThrow (returnStatementOccuredFlag blockEnv) (MissingReturnStatementError position)
    put functionEnv

  checkType _ (IInit _ name variableType expression) = do
    env <- get
    let rawType = fromType variableType
    Utils.assertExpressionType rawType expression
    put $ updateEnvironmentType env (name, rawType)

instance TypeChecker Block where
  checkType expectedType (SBlock _ stmts) = do
    mapM_ (checkType expectedType) stmts

instance TypeChecker Stmt where
  checkType _ (SEmpty _) = return ()
  
  checkType expected (SBStmt _ block) = do
    env <- get
    returnDefinedBefore <- gets returnStatementOccuredFlag
    checkType expected block
    returnNested <- gets returnStatementOccuredFlag
    put env 
    modify $ updateEnvironmentReturnFlag (returnDefinedBefore || returnNested)
  
  checkType expected (SInit _ i) = checkType expected i
  
  checkType _ (SAss position name expression) = do
    env <- get
    case lookupIdent name env of
      (Just variableType) -> Utils.assertExpressionType variableType expression
      Nothing -> throwError $ UnknownIdentifierError position name
  
  checkType _ (SIncr position name) = do
    Utils.assertVariableType RTInt name position
  
  checkType _ (SDecr position name) = do
    Utils.assertVariableType RTInt name position
  
  checkType (Just expectedType) (SRet _ expression) = do
    Utils.assertExpressionType expectedType expression
    modify $ updateEnvironmentReturnFlag True
  
  checkType Nothing (SRet position _) = do
    throwError $ MissingReturnStatementError position
  
  checkType (Just expectedType) (SRetVoid position) = do
    Utils.assertTypesOrThrow expectedType RTVoid (InvalidReturnTypeError position expectedType RTVoid)
    modify $ updateEnvironmentReturnFlag True
  
  checkType Nothing (SRetVoid position) =
    throwError $ MissingReturnStatementError position
  
  checkType expected (SCond _ expression trueBlock) = do
    Utils.assertExpressionType RTBool expression
    returnDefinedBefore <- gets returnStatementOccuredFlag
    checkType expected trueBlock
    modify $ updateEnvironmentReturnFlag returnDefinedBefore
  
  checkType expected (SCondElse _ expression trueBlock falseBlock) = do
    Utils.assertExpressionType RTBool expression
    returnDefinedBefore <- gets returnStatementOccuredFlag
    withStateT (updateEnvironmentReturnFlag False) (checkType expected trueBlock)
    returnTrueBranch <- gets returnStatementOccuredFlag
    withStateT (updateEnvironmentReturnFlag False) (checkType expected falseBlock)
    returnFalseBranch <- gets returnStatementOccuredFlag
    modify $ updateEnvironmentReturnFlag (returnDefinedBefore || (returnTrueBranch && returnFalseBranch))
  
  checkType expected (SWhile _ expression block) = do
    Utils.assertExpressionType RTBool expression
    checkType expected block
  
  checkType _ (SExp _ expression) = do
    _ <- readType expression
    return ()

instance TypeReader Expr where
  readType (EVar position name) =
    Utils.getExistingSymbolOrThrow name (UnknownIdentifierError position name)
  
  readType (ELitInt _ _) = return RTInt
  
  readType (ELitTrue _) = return RTBool
  
  readType (ELitFalse _) = return RTBool
  
  readType (EString _ _) = return RTString
  
  readType (ENeg _ expression) = do
    Utils.assertExpressionType RTInt expression
    return RTInt
  
  readType (ENot _ expression) = do
    Utils.assertExpressionType RTBool expression
    return RTBool
  
  readType (EApp position name expressions) = do
    symbolType <- Utils.getExistingSymbolOrThrow name (UnknownIdentifierError position name)
    case symbolType of
      (RTFun argumentsTypes returnType) -> do
        _ <- if (length argumentsTypes /= length expressions) then throwError (MissingArgumentError position) else return ()
        Utils.assertExpressionListTypes argumentsTypes expressions
        return returnType
      _ -> throwError (InvalidApplicationError position name)
  
  readType (EMul _ e1 _ e2) = do
    Utils.assertExpressionsType RTInt e1 e2
    return RTInt
  
  readType (EAdd _ e1 _ e2) = do
    Utils.assertExpressionsType RTInt e1 e2
    return RTInt
  
  readType (ERel _ e1 _ e2) = do
    Utils.assertExpressionsType RTInt e1 e2
    return RTBool
  
  readType (EAnd _ e1 e2) = do
    Utils.assertExpressionsType RTBool e1 e2
    return RTBool
  
  readType (EOr _ e1 e2) = do
    Utils.assertExpressionsType RTBool e1 e2
    return RTBool
  
  readType lambda@(ELambda _ arguments _ _) = do
    Utils.assertValidArguments arguments
    let argumentsWithTypes = getArgumentsWithTypes arguments
    withStateT (`updateEnvironmentTypes` argumentsWithTypes) (checkLambda lambda)
    where
      checkLambda :: Expr -> TypeReaderT
      checkLambda (ELambda position arguments' returnType block) = do
        let functionType = calculateFunctionType arguments' returnType
        _ <- checkType (Just $ fromType returnType) block
        env <- get
        Utils.assertOrThrow (returnStatementOccuredFlag env) (MissingReturnStatementError position)
        return functionType
