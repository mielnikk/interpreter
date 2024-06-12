{-# LANGUAGE FlexibleInstances #-}
module Evaluator.Evaluator where

import Control.Monad.Except
import Control.Monad.State
import qualified Evaluator.Domain.Context as Context
import Evaluator.Domain.Error
import Evaluator.Domain.Monads
import Evaluator.Domain.Value 
import Evaluator.Utils
import Syntax.AbsTortex

instance Evaluator Program where
  eval (PProgram _ inits) = do
    mapM_ eval inits
    eval $ EApp BNFC'NoPosition (Ident "main") []

instance Evaluator Init where
  eval (IFnDef _ name arguments _ block) = do
    ctx <- get
    let functionType = VFun arguments block (Context.environment ctx)
    modify $ Context.insertValue name functionType
    return Dummy
 
  eval (IInit _ name _ expression) = do
    exprVal <- eval expression
    modify $ Context.insertValue name exprVal
    return Dummy

instance Evaluator Block where
  eval (SBlock _ statements) = do
    mapM_ evalIfNotReturned statements
    return Dummy
    where
      evalIfNotReturned stmt = do
        ctx <- get
        if Context.returnFlag ctx then return Dummy else eval stmt

instance Evaluator Stmt where
  eval (SEmpty _) = return VVoid
 
  eval (SBStmt _ block) = evalRollbackEnv $ eval block
 
  eval (SInit _ si) = eval si
 
  eval (SAss _ name expression) = do
    expressionVal <- eval expression
    modify $ Context.updateValue name expressionVal
    return Dummy
 
  eval (SIncr _ name) = do
    evalIntStmt (1 +) name
 
  eval (SDecr _ name) = do
    evalIntStmt (\x -> x - 1) name
 
  eval (SRet _ expression) = do
    expressionVal <- eval expression
    modify $ Context.setReturned expressionVal
    return Dummy

  eval (SRetVoid _) = do
    modify $ Context.setReturned VVoid
    return Dummy
 
  eval (SCond _ expression trueBlock) = do
    expressionVal <- eval expression
    evalRollbackEnv $ if isTrue expressionVal then eval trueBlock else return Dummy
 
  eval (SCondElse _ expression trueBlock falseBlock) = do
    expressionVal <- eval expression
    evalRollbackEnv $ if isTrue expressionVal then eval trueBlock else eval falseBlock
 
  eval while@(SWhile _ expression block) = do
    expressionVal <- eval expression
    evalRollbackEnv $ if isTrue expressionVal then eval block >> eval while else return Dummy
 
  eval (SExp _ expression) = eval expression

instance Evaluator Expr where
  eval (EVar _ name) = gets $ Context.getValue name
 
  eval (ELitInt _ value) = return $ VInt value
 
  eval (ELitTrue _) = return $ VBool True
 
  eval (ELitFalse _) = return $ VBool False
 
  eval (EString _ value) = return $ VString value
 
  eval (ENeg _ expression) = do
    expressionVal <- eval expression
    return $ mapVInt ((-1) *) expressionVal
 
  eval (ENot _ expression) = do
    expressionVal <- eval expression
    return $ mapVBool not expressionVal
 
  eval (EMul _ e1 (Mod _) e2) = evalIntExpr mod e1 e2
 
  eval (EMul _ e1 (Times _) e2) = evalIntExpr (*) e1 e2
 
  eval (EMul position e1 (Div _) e2) = do
    val2 <- eval e2
    if isIntEqual val2 0 then throwError (DivideByZeroError position) else evalIntExpr quot e1 e2
 
  eval (EAdd _ e1 (Plus _) e2) = evalIntExpr (+) e1 e2
 
  eval (EAdd _ e1 (Minus _) e2) = evalIntExpr (-) e1 e2
 
  eval (ERel _ e1 op e2) = evalIntBoolExpr (getOperation op) e1 e2
 
  eval (EAnd _ e1 e2) = evalBoolExpr (&&) e1 e2
 
  eval (EOr _ e1 e2) = evalBoolExpr (||) e1 e2

  eval (ELambda _ arguments _ block) = gets (VFun arguments block . Context.environment)

  eval (EApp position name expressions) = evalWithBuiltinCheck name expressions position $ do 
    ctx <- get
    argumentVals <- mapM eval expressions
    argumentLocs <- mapM getArgumentLocation expressions

    let env = Context.environment ctx
    let function = Context.getValue name ctx
    let functionEnv = getFunctionEnvironment function
    let functionArgs = getFunctionArgs function
    let functionBlock = getFunctionBlock function

    modify $ Context.insertEnvironment functionEnv
    modify $ Context.insertValue name function
    modify $ Context.setReturnFlag False

    mapM_ insertArgsToCtx (zip3 functionArgs argumentVals argumentLocs)

    _ <- eval functionBlock
    
    newCtx <- get
    let returnValue = Context.getReturnValue newCtx

    modify $ Context.setReturnFlag False
    modify $ Context.insertEnvironment env
    return returnValue
